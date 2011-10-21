package gov.nih.mipav.model.algorithms.t2mapping.cj.roi;

import gov.nih.mipav.model.algorithms.t2mapping.cj.roi.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.measure.*;
import gov.nih.mipav.model.algorithms.t2mapping.cj.volume.*;

class speedles {
public static void main(String argv[])
{
    VoxelInfo vi = new VoxelInfo(); vi.xSize = vi.ySize = 0.88; vi.zSize = 5.0;
    vi.xDimension = vi.yDimension = 256; vi.zDimension = 24;
	RegionList r = new LESList("/usr1/analysis/SA03/gina/qcdone/A1927S05.les", vi);
	//RegionList r = new LESList("/usr1/analysis/saf/gina/qcdone/A2116S03.les", vi);
long t = System.currentTimeMillis();
    Volume v = r.getVolume();
long t2 = System.currentTimeMillis();
System.out.println((double)(t2-t)/1000.0);
System.out.println(Statistics.sum(v));
}
}
