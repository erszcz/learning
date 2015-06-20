// ASCII art renderer.
// Demonstrates traits, impls, non-copyable struct, unit testing.
// To run execute: rustc --test art.rs && ./art

// Rust's core library is tightly bound to the language itself so it is automatically linked in.
// However the std library is designed to be optional (for code that must run on constrained
// environments like embedded devices or special environments like kernel code) so it must
// be explicitly linked in.
extern mod std;

// Extern mod controls linkage. Use controls the visibility of names to modules that are
// already linked in. Using WriterUtil allows us to use the write_line method.
use io::WriterUtil;

// Represents a position on a canvas.
struct Point
{
    x: int,
    y: int,
}

// Represents an offset on a canvas. (This has the same structure as a Point.
// but different semantics). In a real app we could implement traits like core::ops::Add
// to allow points and rectangles to be offset using code like: `pt + delta`.
struct Size
{
    width: int,
    height: int,
}

// Represents a rectangle on a canvas.
struct Rect
{
    top_left: Point,
    size: Size,
}

// Contains the information needed to do shape rendering via ASCII art.
struct AsciiArt
{
    // Fields are immutable, by default, so making them public won't
    // cause problems (except for making it more difficult to change
    // the names or types of fields).
    width: uint,
    height: uint,
    fill: char,
    priv lines: ~[~[char]]

}

// This struct can be quite large so we'll disable copying: developers need
// to either pass these structs around via borrowed pointers or move them.
impl AsciiArt {
    fn drop(&self) {}
}

// It's common to define a constructor sort of function to create struct instances.
// If there is a canonical constructor it is typically named the same as the type.
// Other constructor-ish functions are typically named from_foo, from_bar, etc. 
fn AsciiArt(width: uint, height: uint, fill: char) -> AsciiArt
{
    // Use anonymous functions to build a vector of vectors containing
    // blank characters for each position in our canvas.
    let lines = do vec::build_sized(height)
        |push|
        {
            for height.times
            {
                let mut line = ~[];
                vec::grow_set(line, width-1, '.', '.');
                push(line);
            }
        };

    // Rust code often returns values by omitting the trailing semi-colon
    // instead of using an explicit return statement.
    AsciiArt {width: width, height: height, fill: fill, lines: lines}
}

// Methods particular to the AsciiArt struct.
impl AsciiArt
{
    fn add_pt(x: int, y: int)
    {
        if x >= 0 && x < self.width as int
        {
            if y >= 0 && y < self.height as int
            {
                // Note that numeric types don't implicitly convert to each other.
                let v = y as uint;
                let h = x as uint;

                // Vector subscripting will normally copy the element, but &v[i]
                // will return a reference which is what we need because the
                // element is: 1) potentially large 2) needs to be modified.
                let row = &self.lines[v];
                row[h] = self.fill;
            }
        }
    }
}

// Allows AsciiArt to be converted to a string using the libcore ToStr trait.
// Note that the %s fmt! specifier will not call this automatically.
impl ToStr for AsciiArt
{
    fn to_str() -> ~str
    {
        // Convert each line into a string.
        let lines = do self.lines.map |line| {str::from_chars(line)};

        // Concatenate the lines together using a new-line.
        str::connect(lines, "\n")
    }
}

// This is similar to an interface in other languages: it defines a protocol which
// developers can implement for arbitrary concrete types.
trait Canvas
{
    fn add_point(shape: Point);
    fn add_rect(shape: Rect);

    // Unlike interfaces traits support default implementations.
    // Which currently ICEs: see https://github.com/mozilla/rust/issues/3563
//    fn add_points(shapes: &[Point])
//    {
//        for shapes.each |pt| {self.add_point(pt)};
//    }
}

// Here we provide an implementation of the Canvas methods for AsciiArt.
// Other implementations could also be provided (e.g. for PDF or Apple's Quartz)
// and code can use them polymorphically via the Canvas trait.
impl AsciiArt : Canvas
{
    fn add_point(shape: Point)
    {
        self.add_pt(shape.x, shape.y);
    }

    fn add_rect(shape: Rect)
    {
        // Add the top and bottom lines.
        for int::range(shape.top_left.x, shape.top_left.x + shape.size.width)
        |x|
        {
            self.add_pt(x, shape.top_left.y);
            self.add_pt(x, shape.top_left.y + shape.size.height - 1);
        }

        // Add the left and right lines.
        for int::range(shape.top_left.y, shape.top_left.y + shape.size.height)
        |y|
        {
            self.add_pt(shape.top_left.x, y);
            self.add_pt(shape.top_left.x + shape.size.width - 1, y);
        }
    }
}

// Rust's unit testing framework is currently a bit under-developed so we'll use
// this little helper. The cfg is a conditional compilation attribute that enables
// check_strs only for unit testing builds.
#[cfg(test)]
pub fn check_strs(actual: &str, expected: &str) -> bool
{
    if actual != expected
    {
        io::stderr().write_line(fmt!("Found:\n%s\nbut expected\n%s", actual, expected));
        return false;
    }
    return true;
}

#[test]
fn test_ascii_art_ctor()
{
    let art = AsciiArt(3, 3, '*');
    assert!(check_strs(art.to_str(), "...\n...\n..."));
}

#[test]
fn test_add_pt()
{
    let art = AsciiArt(3, 3, '*');
    art.add_pt(0, 0);
    art.add_pt(0, -10);
    art.add_pt(1, 2);
    assert!(check_strs(art.to_str(), "*..\n...\n.*."));
}

#[test]
fn test_shapes()
{
    let art = AsciiArt(4, 4, '*');
    art.add_rect(Rect {top_left: Point {x: 0, y: 0}, size: Size {width: 4, height: 4}});
    art.add_point(Point {x: 2, y: 2});
    assert!(check_strs(art.to_str(), "****\n*..*\n*.**\n****"));
}
