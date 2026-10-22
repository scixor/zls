fn Matrix(comptime n: comptime_int) type {
    return struct {
        const Self = @This();
        pub const V = @Vector(n, f32);
        cols: [n]V,

        pub fn identity() Self {}
        pub fn mulVec(self: Self, value: V) V {
            _ = .{ self, value };
        }
        pub fn inverse(self: Self) ?Self {
            _ = self;
        }
    };
}

const Mat2 = Matrix(2);
//    ^^^^ (type)(Matrix(2))
const Mat3 = Matrix(3);
//    ^^^^ (type)(Matrix(3))
const cols2 = Mat2.identity().cols;
//    ^^^^^ ([2]@Vector(2,f32))()
const cols3 = Mat3.identity().cols;
//    ^^^^^ ([3]@Vector(3,f32))()
const vector2 = Mat2.identity().mulVec(undefined);
//    ^^^^^^^ (@Vector(2,f32))()
const vector3 = Mat3.identity().mulVec(undefined);
//    ^^^^^^^ (@Vector(3,f32))()
const inverse2 = Mat2.identity().inverse().?.cols;
//    ^^^^^^^^ ([2]@Vector(2,f32))()
const again3 = Mat3.identity().cols;
//    ^^^^^^ ([3]@Vector(3,f32))()

fn Buffer(comptime n: usize) type {
    return struct {
        pub const size = n;
        bytes: [n]u8,
    };
}
const size: usize = 4;
const bytes = @as(Buffer(size), undefined).bytes;
//    ^^^^^ ([4]u8)()
const typed_size = Buffer(4).size;
//    ^^^^^^^^^^ (usize)(4)

fn Nested(comptime n: usize, comptime T: type) type {
    return struct {
        const Self = @This();
        pub const Inner = Buffer(n);
        data: [n]T,
        pub fn init() Self {}
    };
}
const nested2 = Nested(2, u8).init().data;
//    ^^^^^^^ ([2]u8)()
const nested3 = Nested(3, f32).init().data;
//    ^^^^^^^ ([3]f32)()
const nested2f = Nested(2, f32).init().data;
//    ^^^^^^^^ ([2]f32)()
const inner = @as(Nested(5, u8).Inner, undefined).bytes;
//    ^^^^^ ([5]u8)()

fn unresolved(comptime n: usize) void {
    const data = @as(Buffer(n), undefined).bytes;
    //    ^^^^ ([?]u8)()
}
