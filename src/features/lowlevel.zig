//! Portable low-level hover annotations:
//! - Static type/struct/packed-struct/union/enum/vector layout (size, align, offsets, padding)
//! - Static local stack variable footprint on function declarations
//! - Optional disassembly sidecar lookup (`diss/index.tsv` or `ZLS_DISASM_INDEX`)

const std = @import("std");
const Ast = std.zig.Ast;
const Allocator = std.mem.Allocator;

const Analyser = @import("../analysis.zig");
const DocumentStore = @import("../DocumentStore.zig");
const InternPool = @import("../analyser/InternPool.zig");
const types = @import("lsp").types;

const ptr_size: u64 = 8;
const ptr_align: u64 = 8;
const max_depth: u8 = 12;
const max_display_fields: usize = 24;

pub const TypeLayout = struct {
    byte_size: u64,
    bit_size: u64,
    alignment: u64,
    exact: bool = true,
};

const FieldLayout = struct {
    name: []const u8,
    offset: u64, // bytes (or bits for packed)
    size: u64, // bytes (or bits for packed)
    alignment: u64,
};

const ContainerKind = enum {
    auto_struct,
    extern_struct,
    packed_struct,
    @"union",
    tagged_union,
    @"enum",
};

const ContainerLayout = struct {
    kind: ContainerKind,
    layout: TypeLayout,
    padding_bytes: u64 = 0,
    fields: []FieldLayout = &.{},
    unresolved_fields: usize = 0,
};

pub fn appendHoverInfo(
    analyser: *Analyser,
    arena: Allocator,
    decl_handle: Analyser.DeclWithHandle,
    maybe_resolved_type: ?Analyser.Type,
    markup_kind: types.MarkupKind,
    doc_strings: *std.ArrayList([]const u8),
) Analyser.Error!void {
    const tree = &decl_handle.handle.tree;

    // 1. Function declaration: static stack locals + optional disassembly sidecar
    if (decl_handle.decl == .ast_node) {
        const node = decl_handle.decl.ast_node;
        const tag = tree.nodeTag(node);
        if (tag == .fn_decl or tag == .fn_proto or tag == .fn_proto_multi or
            tag == .fn_proto_one or tag == .fn_proto_simple)
        {
            if (tag == .fn_decl) {
                if (try formatFnStackInfo(analyser, arena, decl_handle.handle, node, markup_kind)) |s| {
                    try doc_strings.append(arena, s);
                }
            }
            var fn_buf: [1]Ast.Node.Index = undefined;
            if (tree.fullFnProto(&fn_buf, node)) |fp| {
                if (fp.name_token) |name_tok| {
                    const fn_name = tree.tokenSlice(name_tok);
                    if (formatDisasmSidecar(analyser, arena, decl_handle.handle, fn_name, markup_kind)) |s| {
                        try doc_strings.append(arena, s);
                    }
                }
            }
            return;
        }
    }

    // 2. Type or typed variable/field layout
    const resolved_ty = maybe_resolved_type orelse return;
    if (resolved_ty.is_type_val and resolved_ty.data == .container) {
        if (try computeContainerLayout(analyser, arena, resolved_ty.data.container, 0)) |cl| {
            const text = try formatContainerLayout(arena, cl, markup_kind);
            try doc_strings.append(arena, text);
            return;
        }
    }

    // Non-container type or variable/field with a concrete type: concise 1-line layout
    if (try computeTypeLayout(analyser, arena, resolved_ty, 0)) |tl| {
        if (tl.byte_size > 0 or tl.bit_size > 0) {
            const text = try formatScalarLayout(arena, tl, resolved_ty, markup_kind);
            try doc_strings.append(arena, text);
        }
    }
}

fn computeTypeLayout(
    analyser: *Analyser,
    arena: Allocator,
    ty: Analyser.Type,
    depth: u8,
) Analyser.Error!?TypeLayout {
    if (depth >= max_depth) return null;

    switch (ty.data) {
        .pointer => |ptr| return switch (ptr.flags.size) {
            .slice => .{ .byte_size = ptr_size * 2, .bit_size = ptr_size * 16, .alignment = ptr_align },
            .one, .many, .c => .{ .byte_size = ptr_size, .bit_size = ptr_size * 8, .alignment = ptr_align },
        },
        .array => |arr| {
            const count = arr.elem_count orelse return null;
            const elem = (try computeTypeLayout(analyser, arena, arr.elem_ty.*, depth + 1)) orelse return null;
            const total_count = count + @intFromBool(arr.sentinel != .none);
            const total_bytes = std.math.mul(u64, total_count, elem.byte_size) catch return null;
            return .{
                .byte_size = total_bytes,
                .bit_size = std.math.mul(u64, total_bytes, 8) catch total_bytes,
                .alignment = elem.alignment,
                .exact = elem.exact,
            };
        },
        .optional => |child| {
            if (child.data == .pointer and child.data.pointer.flags.size != .slice) {
                return .{ .byte_size = ptr_size, .bit_size = ptr_size * 8, .alignment = ptr_align };
            }
            const elem = (try computeTypeLayout(analyser, arena, child.*, depth + 1)) orelse return null;
            const al = @max(elem.alignment, 1);
            const sz = alignUp(elem.byte_size + 1, al);
            return .{ .byte_size = sz, .bit_size = sz * 8, .alignment = al, .exact = elem.exact };
        },
        .error_union => |eu| {
            const payload = (try computeTypeLayout(analyser, arena, eu.payload.*, depth + 1)) orelse return null;
            if (payload.byte_size == 0) {
                return .{ .byte_size = 2, .bit_size = 16, .alignment = 2, .exact = payload.exact };
            }
            const al = @max(payload.alignment, 2);
            const sz = alignUp(payload.byte_size + 2, al);
            return .{ .byte_size = sz, .bit_size = sz * 8, .alignment = al, .exact = payload.exact };
        },
        .tuple => |elems| {
            var offset: u64 = 0;
            var max_align: u64 = 1;
            var exact = true;
            for (elems) |elem| {
                const el = (try computeTypeLayout(analyser, arena, elem, depth + 1)) orelse return null;
                if (!el.exact) exact = false;
                const al = @max(el.alignment, 1);
                max_align = @max(max_align, al);
                offset = alignUp(offset, al) + el.byte_size;
            }
            const total = alignUp(offset, max_align);
            return .{ .byte_size = total, .bit_size = total * 8, .alignment = max_align, .exact = exact };
        },
        .container => |c| {
            const cl = (try computeContainerLayout(analyser, arena, c, depth + 1)) orelse return null;
            return cl.layout;
        },
        .ip_index => |ip_data| {
            const target_idx = if (ty.is_type_val) (ip_data.index orelse return null) else ip_data.type;
            return computeIpLayout(analyser.ip, target_idx, depth);
        },
        else => return null,
    }
}

fn computeIpLayout(ip: *InternPool, idx: InternPool.Index, depth: u8) ?TypeLayout {
    if (depth >= max_depth or idx == .none) return null;
    return switch (ip.indexToKey(idx)) {
        .simple_type => |st| switch (st) {
            .void, .noreturn, .empty_struct_type => .{ .byte_size = 0, .bit_size = 0, .alignment = 1 },
            .bool => .{ .byte_size = 1, .bit_size = 1, .alignment = 1 },
            .c_char => .{ .byte_size = 1, .bit_size = 8, .alignment = 1 },
            .f16, .c_short, .c_ushort, .anyerror => .{ .byte_size = 2, .bit_size = 16, .alignment = 2 },
            .f32, .c_int, .c_uint => .{ .byte_size = 4, .bit_size = 32, .alignment = 4 },
            .f64, .usize, .isize, .c_long, .c_ulong, .c_longlong, .c_ulonglong => .{
                .byte_size = 8,
                .bit_size = 64,
                .alignment = 8,
            },
            .f80, .f128, .c_longdouble => .{ .byte_size = 16, .bit_size = 128, .alignment = 16 },
            else => null,
        },
        .int_type => |it| intLayout(it.bits),
        .pointer_type => |ptr| switch (ptr.flags.size) {
            .slice => .{ .byte_size = ptr_size * 2, .bit_size = ptr_size * 16, .alignment = ptr_align },
            .one, .many, .c => .{ .byte_size = ptr_size, .bit_size = ptr_size * 8, .alignment = ptr_align },
        },
        .array_type => |arr| {
            const elem = computeIpLayout(ip, arr.child, depth + 1) orelse return null;
            const total_count = arr.len + @intFromBool(arr.sentinel != .none);
            const total_bytes = std.math.mul(u64, total_count, elem.byte_size) catch return null;
            return .{ .byte_size = total_bytes, .bit_size = total_bytes * 8, .alignment = elem.alignment };
        },
        .vector_type => |vec| {
            if (vec.len == std.math.maxInt(u32)) return null;
            const elem = computeIpLayout(ip, vec.child, depth + 1) orelse return null;
            const total_bits = @as(u64, vec.len) * elem.bit_size;
            const raw_bytes = (total_bits + 7) / 8;
            const al = nextPow2(@max(raw_bytes, elem.alignment));
            const sz = alignUp(raw_bytes, al);
            return .{ .byte_size = sz, .bit_size = total_bits, .alignment = al };
        },
        .optional_type => |opt| {
            if (ip.indexToKey(opt.payload_type) == .pointer_type) {
                const ptr = ip.indexToKey(opt.payload_type).pointer_type;
                if (ptr.flags.size != .slice) {
                    return .{ .byte_size = ptr_size, .bit_size = ptr_size * 8, .alignment = ptr_align };
                }
            }
            const elem = computeIpLayout(ip, opt.payload_type, depth + 1) orelse return null;
            const al = @max(elem.alignment, 1);
            const sz = alignUp(elem.byte_size + 1, al);
            return .{ .byte_size = sz, .bit_size = sz * 8, .alignment = al };
        },
        .error_set_type => .{ .byte_size = 2, .bit_size = 16, .alignment = 2 },
        .error_union_type => |eu| {
            const payload = computeIpLayout(ip, eu.payload_type, depth + 1) orelse return null;
            if (payload.byte_size == 0) return .{ .byte_size = 2, .bit_size = 16, .alignment = 2 };
            const al = @max(payload.alignment, 2);
            const sz = alignUp(payload.byte_size + 2, al);
            return .{ .byte_size = sz, .bit_size = sz * 8, .alignment = al };
        },
        else => null,
    };
}

fn intLayout(bits: u16) TypeLayout {
    if (bits == 0) return .{ .byte_size = 0, .bit_size = 0, .alignment = 1 };
    const bytes: u64 = nextPow2((@as(u64, bits) + 7) / 8);
    const clamped_align: u64 = @min(bytes, 16);
    const clamped_size: u64 = if (bits <= 128) bytes else alignUp((@as(u64, bits) + 7) / 8, 16);
    return .{ .byte_size = clamped_size, .bit_size = bits, .alignment = clamped_align };
}

fn computeContainerLayout(
    analyser: *Analyser,
    arena: Allocator,
    container: Analyser.Type.Data.Container,
    depth: u8,
) Analyser.Error!?ContainerLayout {
    if (depth >= max_depth) return null;
    const handle = container.scope_handle.handle;
    const tree = &handle.tree;
    const doc_scope = try handle.getDocumentScope();
    const ast_node = doc_scope.getScopeAstNode(container.scope_handle.scope) orelse return null;

    var buf: [2]Ast.Node.Index = undefined;
    const decl = tree.fullContainerDecl(&buf, ast_node) orelse return null;
    const main_tok_tag = tree.tokenTag(decl.ast.main_token);

    if (main_tok_tag == .keyword_enum) {
        if (decl.ast.arg.unwrap()) |arg_node| {
            if (try analyser.resolveTypeOfNode(.of(arg_node, handle))) |backing| {
                if (try computeTypeLayout(analyser, arena, backing, depth + 1)) |tl| {
                    return .{ .kind = .@"enum", .layout = tl };
                }
            }
        }
        var field_count: u64 = 0;
        for (decl.ast.members) |m| {
            if (tree.fullContainerField(m) != null) field_count += 1;
        }
        const bits: u16 = if (field_count <= 1) 0 else if (field_count <= 256) 8 else if (field_count <= 65536) 16 else 32;
        return .{ .kind = .@"enum", .layout = intLayout(bits) };
    }

    if (main_tok_tag != .keyword_struct and main_tok_tag != .keyword_union) return null;

    const layout_tok = decl.layout_token;
    const is_extern = if (layout_tok) |lt| tree.tokenTag(lt) == .keyword_extern else false;
    const is_packed = if (layout_tok) |lt| blk: {
        const s = tree.tokenSlice(lt);
        break :blk std.mem.eql(u8, s, "packed") or std.mem.eql(u8, s, "bitpack");
    } else false;

    const kind: ContainerKind = if (main_tok_tag == .keyword_union)
        (if (decl.ast.enum_token != null or decl.ast.arg.unwrap() != null) .tagged_union else .@"union")
    else if (is_packed)
        .packed_struct
    else if (is_extern)
        .extern_struct
    else
        .auto_struct;

    var raw_fields: std.ArrayList(FieldLayout) = .empty;
    var unresolved: usize = 0;

    for (decl.ast.members) |member| {
        const field = tree.fullContainerField(member) orelse continue;
        if (field.comptime_token != null) continue;
        const type_node = field.ast.type_expr.unwrap() orelse {
            if (kind == .tagged_union or kind == .@"union") {
                try raw_fields.append(arena, .{
                    .name = tree.tokenSlice(field.ast.main_token),
                    .offset = 0,
                    .size = 0,
                    .alignment = 1,
                });
            }
            continue;
        };
        const field_ty = (try analyser.resolveTypeOfNode(.of(type_node, handle))) orelse {
            unresolved += 1;
            continue;
        };
        const fl = (try computeTypeLayout(analyser, arena, field_ty, depth + 1)) orelse {
            unresolved += 1;
            continue;
        };
        const name = if (field.ast.tuple_like) "_" else tree.tokenSlice(field.ast.main_token);
        try raw_fields.append(arena, .{
            .name = name,
            .offset = 0,
            .size = if (kind == .packed_struct) fl.bit_size else fl.byte_size,
            .alignment = @max(fl.alignment, 1),
        });
    }

    if (raw_fields.items.len == 0 and unresolved > 0) return null;

    switch (kind) {
        .packed_struct => {
            var bit_off: u64 = 0;
            for (raw_fields.items) |*f| {
                f.offset = bit_off;
                bit_off += f.size;
            }
            const backing = intLayout(@intCast(@min(bit_off, std.math.maxInt(u16))));
            return .{
                .kind = .packed_struct,
                .layout = .{
                    .byte_size = backing.byte_size,
                    .bit_size = bit_off,
                    .alignment = backing.alignment,
                    .exact = unresolved == 0,
                },
                .fields = raw_fields.items,
                .unresolved_fields = unresolved,
            };
        },
        .extern_struct, .auto_struct => {
            if (kind == .auto_struct) {
                // Zig reorders non-extern struct fields by descending alignment (stable).
                std.mem.sort(FieldLayout, raw_fields.items, {}, struct {
                    fn lessThan(_: void, a: FieldLayout, b: FieldLayout) bool {
                        return a.alignment > b.alignment;
                    }
                }.lessThan);
            }
            var offset: u64 = 0;
            var max_align: u64 = 1;
            var data_bytes: u64 = 0;
            for (raw_fields.items) |*f| {
                max_align = @max(max_align, f.alignment);
                offset = alignUp(offset, f.alignment);
                f.offset = offset;
                offset += f.size;
                data_bytes += f.size;
            }
            const total = alignUp(offset, max_align);
            return .{
                .kind = kind,
                .layout = .{
                    .byte_size = total,
                    .bit_size = total * 8,
                    .alignment = max_align,
                    .exact = (kind == .extern_struct) and (unresolved == 0),
                },
                .padding_bytes = total -| data_bytes,
                .fields = raw_fields.items,
                .unresolved_fields = unresolved,
            };
        },
        .@"union", .tagged_union => {
            var max_sz: u64 = 0;
            var max_align: u64 = 1;
            for (raw_fields.items) |f| {
                max_sz = @max(max_sz, f.size);
                max_align = @max(max_align, f.alignment);
            }
            const tag_bytes: u64 = if (kind == .tagged_union)
                (if (raw_fields.items.len <= 256) 1 else if (raw_fields.items.len <= 65536) 2 else 4)
            else
                0;
            const total = alignUp(max_sz + tag_bytes, max_align);
            return .{
                .kind = kind,
                .layout = .{
                    .byte_size = total,
                    .bit_size = total * 8,
                    .alignment = max_align,
                    .exact = unresolved == 0,
                },
                .padding_bytes = total -| (max_sz + tag_bytes),
                .fields = raw_fields.items,
                .unresolved_fields = unresolved,
            };
        },
        .@"enum" => unreachable,
    }
}

fn formatContainerLayout(
    arena: Allocator,
    cl: ContainerLayout,
    markup_kind: types.MarkupKind,
) Allocator.Error![]const u8 {
    var out: std.ArrayList(u8) = .empty;
    const is_md = markup_kind == .markdown;

    if (cl.kind == .packed_struct) {
        try out.print(arena, "{s}Layout (packed):{s} `{d} bits` ({d} B, align {d})", .{
            if (is_md) "**" else "",
            if (is_md) "**" else "",
            cl.layout.bit_size,
            cl.layout.byte_size,
            cl.layout.alignment,
        });
    } else {
        const label = switch (cl.kind) {
            .extern_struct => "extern struct",
            .auto_struct => "struct, est. auto-layout",
            .@"union" => "union",
            .tagged_union => "tagged union",
            .@"enum" => "enum",
            .packed_struct => unreachable,
        };
        const cache_lines = (cl.layout.byte_size + 63) / 64;
        try out.print(arena, "{s}Layout ({s}):{s} `{d} B`, align `{d}`", .{
            if (is_md) "**" else "",
            label,
            if (is_md) "**" else "",
            cl.layout.byte_size,
            cl.layout.alignment,
        });
        if (cl.padding_bytes > 0) {
            try out.print(arena, ", pad `{d} B`", .{cl.padding_bytes});
        }
        if (cl.layout.byte_size > 64) {
            try out.print(arena, " ({d} cache lines)", .{cache_lines});
        }
    }
    if (cl.unresolved_fields > 0) {
        try out.print(arena, " *(+{d} unresolved)*", .{cl.unresolved_fields});
    }

    if (cl.fields.len > 0 and cl.kind != .@"enum") {
        try out.appendSlice(arena, if (is_md) "\n```txt\n" else "\n");
        const shown = @min(cl.fields.len, max_display_fields);
        for (cl.fields[0..shown]) |f| {
            if (cl.kind == .packed_struct) {
                const hi = if (f.size > 0) f.offset + f.size - 1 else f.offset;
                try out.print(arena, "  [{d:3}..{d:<3}] ({d:2}b)  {s}\n", .{ f.offset, hi, f.size, f.name });
            } else {
                try out.print(arena, "  +{d:<4} ({d:4} B, a={d:<2})  {s}\n", .{ f.offset, f.size, f.alignment, f.name });
            }
        }
        if (cl.fields.len > shown) {
            try out.print(arena, "  ... (+{d} more fields)\n", .{cl.fields.len - shown});
        }
        if (is_md) try out.appendSlice(arena, "```");
    }

    return out.items;
}

fn formatScalarLayout(
    arena: Allocator,
    tl: TypeLayout,
    ty: Analyser.Type,
    markup_kind: types.MarkupKind,
) Allocator.Error![]const u8 {
    const prefix = if (!tl.exact) "~" else "";
    const is_vec = ty.data == .ip_index and blk: {
        const idx = if (ty.is_type_val) (ty.data.ip_index.index orelse break :blk false) else ty.data.ip_index.type;
        break :blk idx != .none;
    };
    _ = is_vec;
    if (markup_kind == .markdown) {
        return try arena.print("**Layout:** `{s}{d} B`, align `{d}`", .{ prefix, tl.byte_size, tl.alignment });
    }
    return try arena.print("Layout: {s}{d} B, align {d}", .{ prefix, tl.byte_size, tl.alignment });
}

fn formatFnStackInfo(
    analyser: *Analyser,
    arena: Allocator,
    handle: *DocumentStore.Handle,
    fn_decl_node: Ast.Node.Index,
    markup_kind: types.MarkupKind,
) Analyser.Error!?[]const u8 {
    const tree = &handle.tree;
    const body_node = tree.nodeData(fn_decl_node).node_and_node[1];
    const body_first = tree.firstToken(body_node);
    const body_last = tree.lastToken(body_node);

    var total_bytes: u64 = 0;
    var var_count: usize = 0;
    var unresolved: usize = 0;
    var largest_bytes: u64 = 0;
    var largest_name: []const u8 = "";

    for (1..tree.nodes.len) |raw_idx| {
        const n: Ast.Node.Index = @enumFromInt(raw_idx);
        const tag = tree.nodeTag(n);
        if (tag != .local_var_decl and tag != .simple_var_decl and tag != .aligned_var_decl) continue;
        const tok = tree.nodeMainToken(n);
        if (tok < body_first or tok > body_last) continue;

        const vd = tree.fullVarDecl(n) orelse continue;
        if (tree.tokenTag(vd.ast.mut_token) != .keyword_var) continue;

        var_count += 1;
        const resolved_ty = blk: {
            if (vd.ast.type_node.unwrap()) |tn| {
                if (try analyser.resolveTypeOfNode(.of(tn, handle))) |t| break :blk t;
            }
            if (vd.ast.init_node.unwrap()) |init_n| {
                if (try analyser.resolveTypeOfNode(.of(init_n, handle))) |t| break :blk t;
            }
            break :blk null;
        };
        const ty = resolved_ty orelse {
            unresolved += 1;
            continue;
        };
        const tl = (try computeTypeLayout(analyser, arena, ty, 0)) orelse {
            unresolved += 1;
            continue;
        };
        total_bytes += tl.byte_size;
        if (tl.byte_size > largest_bytes) {
            largest_bytes = tl.byte_size;
            largest_name = tree.tokenSlice(vd.ast.mut_token + 1);
        }
    }

    if (var_count == 0) return null;

    var out: std.ArrayList(u8) = .empty;
    const bold = if (markup_kind == .markdown) "**" else "";
    try out.print(arena, "{s}Stack locals (static):{s} `~{d} B` ({d} vars", .{ bold, bold, total_bytes, var_count });
    if (largest_bytes >= 64 and largest_name.len > 0) {
        try out.print(arena, ", largest: `{s}` {d} B", .{ largest_name, largest_bytes });
    }
    if (unresolved > 0) {
        try out.print(arena, ", +{d} unresolved", .{unresolved});
    }
    try out.append(arena, ')');
    if (total_bytes >= 4096) {
        try out.appendSlice(arena, " [large stack]");
    }
    return out.items;
}

fn formatDisasmSidecar(
    analyser: *Analyser,
    arena: Allocator,
    handle: *DocumentStore.Handle,
    fn_name: []const u8,
    markup_kind: types.MarkupKind,
) ?[]const u8 {
    const file_path = handle.uri.toFsPath(arena) catch return null;
    const index_contents = readDisasmIndex(analyser.store.io, arena, file_path) orelse return null;

    var out: std.ArrayList(u8) = .empty;
    var matched: usize = 0;
    const suffix = std.fmt.allocPrint(arena, ".{s}", .{fn_name}) catch return null;

    var it = std.mem.splitScalar(u8, index_contents, '\n');
    while (it.next()) |line| {
        if (line.len == 0 or line[0] == '#') continue;
        var cols = std.mem.splitScalar(u8, line, '\t');
        const bin = cols.next() orelse continue;
        const sym = cols.next() orelse continue;
        const bytes = cols.next() orelse continue;
        const insns = cols.next() orelse continue;
        const frame = cols.next() orelse continue;
        const calls = cols.next() orelse continue;
        const zmm = cols.next() orelse continue;
        const ymm = cols.next() orelse continue;
        const xmm = cols.next() orelse continue;
        const scalar_fp = cols.next() orelse continue;
        const ipc = cols.next() orelse continue;
        const rtp = cols.next() orelse continue;
        const bottleneck = cols.next() orelse "";

        if (!std.mem.eql(u8, sym, fn_name) and !std.mem.endsWith(u8, sym, suffix)) continue;

        if (matched > 0) out.append(arena, '\n') catch return null;
        const bold = if (markup_kind == .markdown) "**" else "";
        out.print(
            arena,
            "{s}Disasm ({s}):{s} `{s} B` | `{s} insns` | frame `{s} B` | calls `{s}` | SIMD zmm={s} ymm={s} xmm={s} fp={s} | IPC `{s}` rtp `{s}`",
            .{ bold, bin, bold, bytes, insns, frame, calls, zmm, ymm, xmm, scalar_fp, ipc, rtp },
        ) catch return null;
        if (bottleneck.len > 0) {
            out.print(arena, " | `{s}`", .{bottleneck}) catch return null;
        }
        matched += 1;
        if (matched >= 3) break;
    }

    if (matched == 0) return null;
    return out.items;
}

fn readDisasmIndex(io: std.Io, arena: Allocator, file_path: []const u8) ?[]u8 {
    // Walk up from the source file's directory looking for `diss/index.tsv`
    var cur = std.fs.path.dirname(file_path) orelse return null;
    var levels: usize = 0;
    while (levels < 10) : (levels += 1) {
        const candidate = std.fs.path.join(arena, &.{ cur, "diss", "index.tsv" }) catch return null;
        if (std.Io.Dir.openFileAbsolute(io, candidate, .{})) |f| {
            defer f.close(io);
            return std.Io.Dir.cwd().readFileAlloc(io, candidate, arena, .limited(256 * 1024)) catch return null;
        } else |_| {}
        const parent = std.fs.path.dirname(cur) orelse break;
        if (std.mem.eql(u8, parent, cur)) break;
        cur = parent;
    }
    return null;
}

fn alignUp(val: u64, alignment: u64) u64 {
    if (alignment <= 1) return val;
    const mask = alignment - 1;
    return (val + mask) & ~mask;
}

fn nextPow2(v: u64) u64 {
    if (v <= 1) return 1;
    return std.math.ceilPowerOfTwo(u64, v) catch 16;
}
