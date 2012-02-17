package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;

class speed {
public static void main(String argv[])
{
	ROI r = new ROI();
	int i;

	for(i=0;i<4000000;i++)
		r.add((short)(i%2000),(short)(i/2000),(short)1,(float)1);
	Runtime rt = Runtime.getRuntime();
    System.out.println("Total Mem = "+rt.totalMemory());
    System.out.println("Free Mem =  "+rt.freeMemory());
    System.out.println("Used Mem =  "+(rt.totalMemory()-rt.freeMemory()));
	rt.gc();
    System.out.println("Total Mem = "+rt.totalMemory());
    System.out.println("Free Mem =  "+rt.freeMemory());
    System.out.println("Used Mem =  "+(rt.totalMemory()-rt.freeMemory()));
/*
	for(i=0;i<r.size();i++) {POI p = r.getPOI(i);
	System.out.println("("+p.x+","+p.y+","+p.z+"): "+p.f);}
*/
	System.out.println(r.contains(-1,-1,1));
	System.out.println(r.contains(0,0,1));
}
}
