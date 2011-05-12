package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;

/*
The templated class for representing a point in 3D space.
The class is templated over the ScalarType class that is used to represent coordinates. All the usual
operator overloading (* + - ...) is present. 
*/
public class Point3 {
	public float x, y, z;
	
	public Point3() {
		
	}
	
	public final void print() {
		System.err.println(" x = " + x + " y = " + y + " z = " + z);
	}
	
	public  Point3(float nx, float ny, float nz) {
		x = nx;
		y = ny;
		z = nz;
	}
	
	public  Point3( Point3 p ) {
		x = p.x;
		y = p.y;
		z = p.z;
	}
	
	public Point3( float[] nv) {
		x = nv[0];
		y = nv[1];
		z = nv[2];
	}
	
	// & operator =
	public  final Point3 assign(Point3 b) {
		x = b.x;
	    y = b.y;
		z = b.z;
		return this;
	}
	
	public  final void zero()
	{
		x = 0;
		y = 0;
		z = 0;
	}
	
	
	// & operator []
	public  final float get(int index) {
		if ( index == 0)
			return x;
		else if ( index == 1 )
			return y;
		else { // index == 2, z
			return z;
		}
	}
	
	public  final float X() {
		return x;
	}
	
	public  final float Y() {
		return y;
	}
	
	public  final float Z() {
		return z;
	}
	
	public  final Point3 V() {
		return this;
	}
	
	// operator + 
	public  final Point3 add(Point3 b) {
		return new Point3(x + b.x, y + b.y, z + b.z);
	}
	
	// operator - 
	public  final Point3 sub(Point3 b) {
		return new Point3(x - b.x, y - b.y, z - b.z);
	}
	
	// operator *
	public  final Point3 mul(float s) {
		return new Point3(x * s, y * s, z * s);
	}
	
	
	// operator /
	public  final Point3 div(float v) {
		return new Point3(x / v, y / v, z / v);
	}
	
	/// Dot product, operator *
	public  final float dot( Point3 p )
	{
		return ( x*p.x + y*p.y + z*p.z );
	}
	
	/// Cross product, operator ^ 
	public  final Point3 cross( Point3 p )
	{
		return new Point3
		(
			y*p.z - z*p.y,
			z*p.x - x*p.z,
			x*p.y - y*p.x
		);
	}
	
	
	// & operator +=
	public  final Point3 add_into(Point3 b) {
		x += b.x;
		y += b.y; 
		z += b.z;
		return this;
	}
	
	// & operator -=
	public  final Point3 sub_into(Point3 b) {
		x -= b.x;
		y -= b.y; 
		z -= b.z;
		return this;
	}
	
	// & operator *=
	public Point3 mul_into(float s) {
		x *= s;
		y *= s; 
		z *= s;
		return this;
	}
	
	// & operator /=
	public  final Point3 div_into(float v) {
		x /= v;
		y /= v; 
		z /= v;
		return this;
	}
	
	// Norme
	public  final float norm()
	{
		return (float)Math.sqrt( x*x + y*y + z*z );
	}
	
	public  final float squaredNorm() 
	{
		return (  x*x + y*y + z*z  );
	}
	
	// Scalatura differenziata
	public  final Point3 scale( float sx, float sy, float sz )
	{
		x *= sx;
		y *= sy;
		z *= sz;
		return this;
	}
	
	public  final Point3 scale( Point3  p )
	{
		x *= p.x;
		y *= p.y;
		x *= p.z;
		return this;
	}
	

	// Normalizzazione
	public  final Point3 normalize()
	{
        float n = (float)Math.sqrt(x*x + y*y + z*z);
		if(n>0.0) {	x /= n;	y /= n;	z /= n;  }
		return this;
	}
	
	
	// operator == 
	public  final boolean equals(Point3 b) {
		return (x == b.x && y == b.y && z == b.z);
	}
	
	// operator !=
	public  final boolean notequals(Point3 b) {
		return (x != b.x || y != b.y || z != b.z);		
	}
	
	// operator <
	public  final boolean lessThan( Point3 p )
	{
		return	(z!=p.z)?(z<p.z):
				(y!=p.y)?(y<p.y):
						       (x<p.x);
	}
	
	// operator >
	public  final boolean greaterThan( Point3 p )
	{
		return	(z!=p.z)?(z>p.z):
				(y!=p.y)?(y>p.y):
							   (x>p.x);
	}
	
	// operator <= 
	public  final boolean lessEqualThan( Point3 p )
	{
		return	(z!=p.z)?(z< p.z):
				(y!=p.y)?(y< p.y):
							   (x<=p.x);
	}
	
	// operator >=
	public  final boolean greaterEqualThan( Point3 p )
	{
		return	(z!=p.z)?(z> p.z):
				(y!=p.y)?(y> p.y):
							   (x>=p.x);
	}
	
	public  final Point3 negate()
	{
		return new Point3( -x, -y, -z );
	}
	
	
	
	
}