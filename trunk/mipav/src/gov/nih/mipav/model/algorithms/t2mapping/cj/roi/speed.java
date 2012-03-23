package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;

import gov.nih.mipav.view.MipavUtil;

class speed {
public static void main(String argv[])
{
	ROI r = new ROI();
	int i;

	for(i=0;i<4000000;i++)
		r.add((short)(i%2000),(short)(i/2000),(short)1,(float)1);
	Runtime rt = Runtime.getRuntime();
	System.err.println("Total Mem = "+MipavUtil.getMaxHeapMemory());
    System.err.println("Free Mem =  "+MipavUtil.getFreeHeapMemory());
    System.err.println("Used Mem =  "+MipavUtil.getUsedHeapMemory());
	rt.gc();
	System.err.println("Total Mem = "+MipavUtil.getMaxHeapMemory());
    System.err.println("Free Mem =  "+MipavUtil.getFreeHeapMemory());
    System.err.println("Used Mem =  "+MipavUtil.getUsedHeapMemory());
/*
	for(i=0;i<r.size();i++) {POI p = r.getPOI(i);
	System.out.println("("+p.x+","+p.y+","+p.z+"): "+p.f);}
*/
	System.out.println(r.contains(-1,-1,1));
	System.out.println(r.contains(0,0,1));
}
}
