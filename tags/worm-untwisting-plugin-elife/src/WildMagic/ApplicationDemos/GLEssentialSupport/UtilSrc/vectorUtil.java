package WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc;

public class vectorUtil
{


	public static void vec4Add(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] + rhs[0];
		vec[1] = lhs[1] + rhs[1];
		vec[2] = lhs[2] + rhs[2];
		vec[3] = lhs[3] + rhs[3];
	}

	public static void vec4Subtract(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] - rhs[0];
		vec[1] = lhs[1] - rhs[1];
		vec[2] = lhs[2] - rhs[2];
		vec[3] = lhs[3] - rhs[3];
	}


	public static void vec4Multiply(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] * rhs[0];
		vec[1] = lhs[1] * rhs[1];
		vec[2] = lhs[2] * rhs[2];
		vec[3] = lhs[3] * rhs[3];
	}

	public static void vec4Divide(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] / rhs[0];
		vec[1] = lhs[1] / rhs[1];
		vec[2] = lhs[2] / rhs[2];
		vec[3] = lhs[3] / rhs[3];
	}


	public static void vec3Add(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] + rhs[0];
		vec[1] = lhs[1] + rhs[1];
		vec[2] = lhs[2] + rhs[2];
	}

	public static void vec3Subtract(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] - rhs[0];
		vec[1] = lhs[1] - rhs[1];
		vec[2] = lhs[2] - rhs[2];
	}


	public static void vec3Multiply(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] * rhs[0];
		vec[1] = lhs[1] * rhs[1];
		vec[2] = lhs[2] * rhs[2];
	}

	public static void vec3Divide(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[0] / rhs[0];
		vec[1] = lhs[1] / rhs[1];
		vec[2] = lhs[2] / rhs[2];
	}

	public static float vec3DotProduct(final float[] lhs, final float[] rhs)
	{
		return lhs[0]*rhs[0] + lhs[1]*rhs[1] + lhs[2]*rhs[2];	
	}

	public static float vec4DotProduct(final float[] lhs, final float[] rhs)
	{
		return lhs[0]*rhs[0] + lhs[1]*rhs[1] + lhs[2]*rhs[2] + lhs[3]*rhs[3];	
	}

	public static void vec3CrossProduct(float[] vec, final float[] lhs, final float[] rhs)
	{
		vec[0] = lhs[1] * rhs[2] - rhs[1] * lhs[2];
		vec[1] = lhs[0] * rhs[2] - rhs[0] * lhs[2];
		vec[2] = lhs[1] * rhs[1] - rhs[1] * lhs[1];
	}

	public static float vec3Length(final float[] vec)
	{
		return (float) Math.sqrt(vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2]);
	}

	public static float vec3Distance(final float[] pointA, final float[] pointB)
	{
		float diffx = pointA[0]-pointB[0];
		float diffy = pointA[1]-pointB[1];
		float diffz = pointA[2]-pointB[2];
		return (float) Math.sqrt(diffx*diffx + diffy*diffy + diffz*diffz);
	}

	public static void vec3Normalize(float[] vec, final float[] src)
	{
		float length = vec3Length(src);

		vec[0] = src[0]/length;
		vec[1] = src[1]/length;
		vec[2] = src[2]/length;
	}

}
