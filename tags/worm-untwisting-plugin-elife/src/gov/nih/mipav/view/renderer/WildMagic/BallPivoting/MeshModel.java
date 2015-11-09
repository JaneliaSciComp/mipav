package gov.nih.mipav.view.renderer.WildMagic.BallPivoting;


public class MeshModel {
	
	public static int MM_NONE							= 0x00000000;
	public static int MM_VERTCOORD				= 0x00000001;
	public static int MM_VERTNORMAL				= 0x00000002;
	public static int MM_VERTFLAG					= 0x00000004;
	public static int MM_VERTCOLOR				= 0x00000008;
	public static int MM_VERTQUALITY			= 0x00000010;
	public static int MM_VERTMARK					= 0x00000020;
	public static int MM_VERTFACETOPO			= 0x00000040;
	public static int MM_VERTCURV					= 0x00000080;
	public static int MM_VERTCURVDIR			= 0x00000100;
	public static int MM_VERTRADIUS				= 0x00000200;
	public static int MM_VERTTEXCOORD			= 0x00000400;

	public static int MM_FACEVERT					= 0x00001000;
	public static int MM_FACENORMAL				= 0x00002000;
	public static int MM_FACEFLAG					= 0x00004000;
	public static int MM_FACECOLOR				= 0x00008000;
	public static int MM_FACEQUALITY			= 0x00010000;
	public static int MM_FACEMARK					= 0x00020000;
	public static int MM_FACEFACETOPO			= 0x00040000;
	public static int MM_WEDGTEXCOORD			= 0x00080000;
	public static int MM_WEDGNORMAL				= 0x00100000;
	public static int MM_WEDGCOLOR				= 0x00200000;

//	SubParts of bits
	public static int MM_VERTFLAGSELECT		= 0x01000000;
	public static int MM_FACEFLAGSELECT		= 0x02000000;
//This part should be deprecated.
	public static int MM_VERTFLAGBORDER		= 0x04000000;
	public static int MM_FACEFLAGBORDER		= 0x08000000;

//Per Mesh Stuff....
	public static int MM_CAMERA						= 0x10000000;

	public static int MM_ALL							= 0xffffffff;
	
	public TriMesh cm = new TriMesh();
	
	public int currentDataMask;;
	
	
	public final void clearDataMask(int unneededDataMask)
	  {
		    /*
			if( ( (unneededDataMask & MM_VERTFACETOPO)!=0)	&& hasDataMask(MM_VERTFACETOPO)) {cm.face.DisableVFAdjacency();
																																												cm.vert.DisableVFAdjacency(); }
			if( ( (unneededDataMask & MM_FACEFACETOPO)!=0)	&& hasDataMask(MM_FACEFACETOPO))	cm.face.DisableFFAdjacency();

			if( ( (unneededDataMask & MM_WEDGTEXCOORD)!=0)	&& hasDataMask(MM_WEDGTEXCOORD)) 	cm.face.DisableWedgeTex();
			if( ( (unneededDataMask & MM_FACECOLOR)!=0)			&& hasDataMask(MM_FACECOLOR))			cm.face.DisableColor();
			if( ( (unneededDataMask & MM_FACEQUALITY)!=0)		&& hasDataMask(MM_FACEQUALITY))		cm.face.DisableQuality();
			if( ( (unneededDataMask & MM_FACEMARK)!=0)			&& hasDataMask(MM_FACEMARK))			cm.face.DisableMark();
			if( ( (unneededDataMask & MM_VERTMARK)!=0)			&& hasDataMask(MM_VERTMARK))			cm.vert.DisableMark();
			if( ( (unneededDataMask & MM_VERTCURV)!=0)			&& hasDataMask(MM_VERTCURV))			cm.vert.DisableCurvature();
			if( ( (unneededDataMask & MM_VERTCURVDIR)!=0)		&& hasDataMask(MM_VERTCURVDIR))		cm.vert.DisableCurvatureDir();
			if( ( (unneededDataMask & MM_VERTRADIUS)!=0)		&& hasDataMask(MM_VERTRADIUS))		cm.vert.DisableRadius();
              */
	    currentDataMask = currentDataMask & (~unneededDataMask);
	  }
	
	 public final boolean hasDataMask(int maskToBeTested)
	{
		return ((currentDataMask & maskToBeTested)!= 0);
	}

	
}