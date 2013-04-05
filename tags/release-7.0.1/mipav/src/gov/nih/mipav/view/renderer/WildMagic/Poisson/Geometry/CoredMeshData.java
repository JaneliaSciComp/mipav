package gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry;

import java.util.*;

public abstract class CoredMeshData {

	public static final int[] IN_CORE_FLAG ={1,2,4};
	
	public Vector<Point3D> inCorePoints = new Vector<Point3D>();
	
	public abstract void resetIterator();
	
	public abstract int addOutOfCorePoint(Point3D p);

	// icFlag = (IN_CORE_FLAG[0] | IN_CORE_FLAG[1] | IN_CORE_FLAG[2]);
	public abstract int addTriangle(final TriangleIndex t, final int icFlag);

	public abstract int nextOutOfCorePoint(Point3D p);

	public abstract int nextTriangle(TriangleIndex t, int inCoreFlag);
	
	public abstract int outOfCorePointCount();

	public abstract int triangleCount();
}