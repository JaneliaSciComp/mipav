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
	
	public void print() {
		System.err.println(" x = " + x + " y = " + y + " z = " + z);
	}
	
	public Point3(float nx, float ny, float nz) {
		x = nx;
		y = ny;
		z = nz;
	}
	
	public Point3( Point3 p ) {
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
	public Point3 assign(Point3 b) {
		x = b.x;
	    y = b.y;
		z = b.z;
		return this;
	}
	
	public void Zero()
	{
		x = 0;
		y = 0;
		z = 0;
	}
	
	
	// & operator []
	public float get(int index) {
		if ( index == 0)
			return x;
		else if ( index == 1 )
			return y;
		else { // index == 2, z
			return z;
		}
	}
	
	public float X() {
		return x;
	}
	
	public float Y() {
		return y;
	}
	
	public float Z() {
		return z;
	}
	
	public Point3 V() {
		return this;
	}
	
	// operator + 
	public Point3 add(Point3 b) {
		return new Point3(x + b.x, y + b.y, z + b.z);
	}
	
	// operator - 
	public Point3 sub(Point3 b) {
		return new Point3(x - b.x, y - b.y, z - b.z);
	}
	
	// operator *
	public Point3 mul(float s) {
		return new Point3(x * s, y * s, z * s);
	}
	
	
	// operator /
	public Point3 div(float v) {
		return new Point3(x / v, y / v, z / v);
	}
	
	/// Dot product, operator *
	public float dot( Point3 p )
	{
		return ( x*p.x + y*p.y + z*p.z );
	}
	
	/// Cross product, operator ^ 
	public Point3 Cross( Point3 p )
	{
		return new Point3
		(
			y*p.z - z*p.y,
			z*p.x - x*p.z,
			x*p.y - y*p.x
		);
	}
	
	
	// & operator +=
	public Point3 add_into(Point3 b) {
		x += b.x;
		y += b.y; 
		z += b.z;
		return this;
	}
	
	// & operator -=
	public Point3 sub_into(Point3 b) {
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
	public Point3 div_into(float v) {
		x /= v;
		y /= v; 
		z /= v;
		return this;
	}
	
	// Norme
	public float Norm()
	{
		return (float)Math.sqrt( x*x + y*y + z*z );
	}
	
	public float SquaredNorm() 
	{
		return (  x*x + y*y + z*z  );
	}
	
	// Scalatura differenziata
	public Point3 Scale( float sx, float sy, float sz )
	{
		x *= sx;
		y *= sy;
		z *= sz;
		return this;
	}
	
	public Point3 Scale( Point3  p )
	{
		x *= p.x;
		y *= p.y;
		x *= p.z;
		return this;
	}
	

	// Normalizzazione
	public Point3 Normalize()
	{
        float n = (float)Math.sqrt(x*x + y*y + z*z);
		if(n>0.0) {	x /= n;	y /= n;	z /= n;  }
		return this;
	}
	
	
	// operator == 
	public boolean equals(Point3 b) {
		return (x == b.x && y == b.y && z == b.z);
	}
	
	// operator !=
	public boolean notequals(Point3 b) {
		return (x != b.x || y != b.y || z != b.z);		
	}
	
	// operator <
	public boolean lessThan( Point3 p )
	{
		return	(z!=p.z)?(z<p.z):
				(y!=p.y)?(y<p.y):
						       (x<p.x);
	}
	
	// operator >
	public boolean greaterThan( Point3 p )
	{
		return	(z!=p.z)?(z>p.z):
				(y!=p.y)?(y>p.y):
							   (x>p.x);
	}
	
	// operator <= 
	public boolean lessEqualThan( Point3 p )
	{
		return	(z!=p.z)?(z< p.z):
				(y!=p.y)?(y< p.y):
							   (x<=p.x);
	}
	
	// operator >=
	public boolean greaterEqualThan( Point3 p )
	{
		return	(z!=p.z)?(z> p.z):
				(y!=p.y)?(y> p.y):
							   (x>=p.x);
	}
	
	public Point3 negate()
	{
		return new Point3( -x, -y, -z );
	}
	
	
	
	
}