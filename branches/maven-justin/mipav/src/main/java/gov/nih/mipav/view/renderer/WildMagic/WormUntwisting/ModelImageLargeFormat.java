package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

public class ModelImageLargeFormat {
	public static final int INT = 0;
	public static final int ARGB = 1;
	public static final int ARGB_FLOAT = 2;

	private HashMap<Integer, String[]> fileTable;
	private HashMap<Integer, ModelImage> sliceTable;
	private Vector<ModelImage> lruList;
	private boolean[] onDisk;
	private boolean[] modified;
	
	private int[] extents2D;
	private int[] extents3D;
	private String directory;
	private String name;
	private int dataType;
	private int sliceOffset = 0;
	private float[] resolutions;
//	private float zScale = 1;
	private FileIO fileIO = null;
	
	public ModelImageLargeFormat( int[] extents, String fullPathName, boolean readFromDisk )
	{
		this(INT, extents, fullPathName, readFromDisk);
	}
	
	public ModelImageLargeFormat( int type, int[] extents, String fullPathName, boolean readFromDisk )
	{
		this.extents3D = extents;
		extents2D = new int[]{extents[0], extents[1]};
		resolutions = new float[]{1,1,1};
		this.name = fullPathName.substring(fullPathName.lastIndexOf(File.separator) + 1);
		directory = fullPathName.substring(0, fullPathName.lastIndexOf(File.separator) + 1);
		dataType = type;
		
		onDisk = new boolean[extents[2]];
		modified = new boolean[extents[2]];
		for ( int i = 0; i < extents[2]; i++ )
		{
			onDisk[i] = readFromDisk;
			modified[i] = false;
		}
		initFileTable(readFromDisk);
		sliceTable = new HashMap<Integer,ModelImage>();
		lruList = new Vector<ModelImage>();
	}
	
	public void clearTable()
	{
		int numRemove = sliceTable.size();
		for ( int i = 0; i < numRemove; i++ )
    	{
    		ModelImage removeSlice = lruList.lastElement();
    		Iterator<Integer> iterator = sliceTable.keySet().iterator();
    		while ( iterator.hasNext() )
    		{
    			Integer key = iterator.next();
    			if ( sliceTable.get(key) == removeSlice )
    			{
    				sliceTable.remove(key);
    				lruList.remove(removeSlice);
    				if ( modified[key] )
    				{
    					String[] fileInfo = fileTable.get(key);
    					//        				System.err.println( "updateSliceTable " + key + " " + fileInfo[0] + " " + fileInfo[1] );
    					removeSlice.setImageName( fileInfo[0] );
    					saveImage( removeSlice, fileInfo[0], fileInfo[1] );
    					modified[key] = false;
    					onDisk[key] = true;
    				}
    				//        			System.err.println( "updateSliceTable removing from table: " + key );
    				break;
    			}
    		}
    		removeSlice.disposeLocal(false);
    		removeSlice = null;
    	}
    	System.gc();
	}
	
	public void disposeLocal( boolean gc )
	{
		for ( int i = 0; i < extents3D[2]; i++ )
		{
			ModelImage slice = sliceTable.get(i);
			if ( slice != null )
			{
				slice.disposeLocal(false);
			}
		}
		extents2D = null;
		extents3D = null;
		directory = null;
		onDisk = null;
		modified = null;
		if ( gc )
		{
			System.gc();
		}
	}
	
	public int[] getExtents()
	{
		return extents3D;
	}
	
	public static int getIndex( String fileName )
	{
		StringTokenizer st = new StringTokenizer(fileName, ".");
		if (st.hasMoreTokens()) {
			String name = st.nextToken();
			int start = 0;
			int end = name.length();
			boolean found = false;
			for ( int i = name.length()-1; i >= 0; i-- )
			{
				if ( Character.isDigit(name.charAt(i)) )
				{
					found = true;
				}
				if ( !Character.isDigit(name.charAt(i)) )
				{
					if ( !found )
					{
						end--;
					}
					else if ( found )
					{
						start = i+1;
						break;
					}
				}
			}
			String temp = name.substring(start, end);
			int value = Integer.valueOf(temp);
			return value;
		}
		return -1;
	}
	
	public String getHomeDirectory()
	{
		String dirName = directory + name + File.separator;
//		System.err.println(dirName);
		return dirName;
	}
	
	public String getOutputDirectory()
	{
		String dirName = directory + name + "_results" + File.separator;
//		System.err.println(dirName);
		return dirName;
	}
	
	public String getImageName()
	{
		return name;
	}
	
	public int getType()
	{
		return dataType;
	}
	
//	public void set(int x, int y, int z, int value )
//	{
//		ModelImage slice = updateSliceTable(z);
//		if ( slice != null )
//		{
//			slice.set(x, y, value);
//			modified[z] = true;
//		}
//	}
//
//	
//	public void set(int x, int y, int z, float[] value )
//	{
//		ModelImage slice = updateSliceTable(z);
//		if ( slice != null )
//		{
//			slice.setC(x, y, 0, value[0]);
//			slice.setC(x, y, 1, value[1]);
//			slice.setC(x, y, 2, value[2]);
//			slice.setC(x, y, 3, value[3]);
//			modified[z] = true;			
//		}
//	}
	
	public int get(int x, int y, int z )
	{
		if ( resolutions[2] != 1 )
			return getInterpolated(x, y, z);
		ModelImage slice = updateSliceTable(z);
		if ( slice != null )
		{
			return slice.getInt(x, y);
		}
		return 0;
	}

	private int getInterpolated(int x, int y, int z)
	{
		x -= extents3D[0]/2;
		y -= extents3D[1]/2;

		x /= resolutions[0];
		y /= resolutions[1];

		x += extents3D[0]/2;
		y += extents3D[1]/2;
		
		int z1 = (int) Math.floor(z / resolutions[2]);
		int z2 = (int) Math.ceil(z / resolutions[2]);
		float fraction = (z / resolutions[2]) - z1;
		float z1Portion = (1 - fraction);
		float z2Portion = fraction;
		if ( z1Portion == 1 )
		{
			ModelImage slice = updateSliceTable(z1);
			if ( slice != null )
			{
				return slice.getInt(x, y);
			}			
		}
		if ( z2Portion == 1 )
		{
			ModelImage slice = updateSliceTable(z2);
			if ( slice != null )
			{
				return slice.getInt(x, y);
			}			
		}

		ModelImage slice1 = updateSliceTable(z1);
		ModelImage slice2 = updateSliceTable(z2);
		if ( (slice1 != null) && (slice2 != null) )
		{
			return Math.round( z1Portion * slice1.getInt(x,y) + z2Portion * slice2.getInt(x,y) );
		}
		return 0;
	}

	private float[] getInterpolatedC(int x, int y, int z)
	{
		x -= extents3D[0]/2;
		y -= extents3D[1]/2;

		x /= resolutions[0];
		y /= resolutions[1];

		x += extents3D[0]/2;
		y += extents3D[1]/2;
		
		int z1 = (int) Math.floor(z / resolutions[2]);
		int z2 = (int) Math.ceil(z / resolutions[2]);
		float fraction = (z / resolutions[2]) - z1;
		float z1Portion = (1 - fraction);
		float z2Portion = fraction;
		if ( z1Portion == 1 )
		{
			ModelImage slice = updateSliceTable(z1);
			if ( slice != null )
			{
				float a = slice.getFloatC(x, y, 0);
				float r = slice.getFloatC(x, y, 1);
				float g = slice.getFloatC(x, y, 2);
				float b = slice.getFloatC(x, y, 3);
				return new float[]{a,r,g,b};	
			}			
		}
		if ( z2Portion == 1 )
		{
			ModelImage slice = updateSliceTable(z2);
			if ( slice != null )
			{
				float a = slice.getFloatC(x, y, 0);
				float r = slice.getFloatC(x, y, 1);
				float g = slice.getFloatC(x, y, 2);
				float b = slice.getFloatC(x, y, 3);
				return new float[]{a,r,g,b};	
			}			
		}

		ModelImage slice1 = updateSliceTable(z1);
		ModelImage slice2 = updateSliceTable(z2);
		if ( (slice1 != null) && (slice2 != null) )
		{
			float a = z1Portion * slice1.getFloatC(x, y, 0) + z2Portion * slice2.getFloatC(x, y, 0);
			float r = z1Portion * slice1.getFloatC(x, y, 1) + z2Portion * slice2.getFloatC(x, y, 1);
			float g = z1Portion * slice1.getFloatC(x, y, 2) + z2Portion * slice2.getFloatC(x, y, 2);
			float b = z1Portion * slice1.getFloatC(x, y, 3) + z2Portion * slice2.getFloatC(x, y, 3);
			return new float[]{a,r,g,b};	
		}
		return new float[]{0,0,0,0};	
	}
	
	public float[] getC(int x, int y, int z )
	{
		if ( resolutions[2] != 1 )
			return getInterpolatedC(x, y, z);
		ModelImage slice = updateSliceTable(z);
		if ( slice != null )
		{
			float a = slice.getFloatC(x, y, 0);
			float r = slice.getFloatC(x, y, 1);
			float g = slice.getFloatC(x, y, 2);
			float b = slice.getFloatC(x, y, 3);
			return new float[]{a,r,g,b};			
		}
		return new float[]{0,0,0,0};	
	}
	
//	public void save(String dir)
//	{
//		System.err.println("saving " + dir );
//		
//		for ( int i = 0; i < extents3D[2]; i++ )
//		{
//			int index = i + sliceOffset;
//			if ( !onDisk[index] || modified[index] )
//			{
//				ModelImage slice = updateSliceTable(i);
//				if ( slice != null )
//				{
//					saveImage(slice, name + "_" + i, dir);
//				}
//				else
//				{
//					if ( dataType == INT )
//					{
//						slice = new ModelImage(ModelStorageBase.INTEGER, extents2D, "Slice_" + i );
//					}
//					else if ( dataType == ARGB )
//					{
//						slice = new ModelImage(ModelStorageBase.ARGB_FLOAT, extents2D, "Slice_" + i );
//					}
//					else if ( dataType == ARGB_FLOAT )
//					{
//						slice = new ModelImage(ModelStorageBase.ARGB_FLOAT, extents2D, "Slice_" + i );
//					}
//					saveImage(slice, name + "_" + i, dir);
//					slice.disposeLocal();
//					slice = null;
//				}
//			}
//		}		
//	}
	
//	public void reslize()
//	{
//		Vector3f[][] sliceExtents = new Vector3f[extents3D[2]][2];
//		Vector3f pt = new Vector3f();
//		for ( int z = 0; z < extents3D[2]; z++ )
//		{
//			sliceExtents[z][0] = new Vector3f(extents3D[0], extents3D[1], extents3D[2]); 
//			sliceExtents[z][1] = new Vector3f(0, 0, 0); 
//			for ( int y = 0; y < extents3D[1]; y++ )
//			{
//				for ( int x = 0; x < extents3D[0]; x++ )
//				{
//					int value = get(x,y,z);
//					if ( value > 0 )
//					{
//						pt.set(x,y,z);
//						sliceExtents[z][0].min(pt);
//						sliceExtents[z][1].max(pt);
//					}
//				}
//			}
//			System.err.println( z + "    " + sliceExtents[z][0] + "        " + sliceExtents[z][1] );
//		}
//		
//		
//		System.err.println( extents3D[2] + " " + extents3D[1] + " " + extents3D[0] );
//		
//		String dirName = directory + name + "_reslice" + File.separator;
//		ModelImage temp = new ModelImage( ModelStorageBase.INTEGER, new int[]{ extents3D[2], extents3D[1] }, "Slice_" );
//		
//		for ( int x = 0; x < extents3D[0]; x++ )
//		{
//			System.err.print( "reslice... " + x );
//			for ( int z = 0; z < extents3D[2]; z++ )
//			{
////				System.err.print( z + "..." );
//				for ( int y = 0; y < extents3D[1]; y++ )
//				{
//					if ( (sliceExtents[z][0].X <= x) && (sliceExtents[z][0].Y <= y) && (sliceExtents[z][0].Z <= z) &&
//						 (sliceExtents[z][1].X >= x) && (sliceExtents[z][1].Y >= y) && (sliceExtents[z][1].Z >= z)    )
//					{
//						temp.set(z, y, get(x, y, z) );
//					}
//					else
//					{
//						temp.set(z, y, 0);
//					}
//				}
//			}
//			temp.setImageName( "Slice_" + x );
//			saveImage(temp, temp.getImageName(), dirName );
//			System.err.println( "... done" );
//			
//			x++;
//			if ( x == extents3D[0] )
//			{
//				break;
//			}
//			System.err.print( "reslice... " + x );
//			for ( int z = extents3D[2] - 1; z >= 0; z-- )
//			{
////				System.err.print( z + "..." );
//				for ( int y = 0; y < extents3D[1]; y++ )
//				{
//					if ( (sliceExtents[z][0].X <= x) && (sliceExtents[z][0].Y <= y) && (sliceExtents[z][0].Z <= z) &&
//						 (sliceExtents[z][1].X >= x) && (sliceExtents[z][1].Y >= y) && (sliceExtents[z][1].Z >= z)    )
//					{
//						temp.set(z, y, get(x, y, z) );
//					}
//					else
//					{
//						temp.set(z, y, 0);
//					}
//				}
//			}
//			temp.setImageName( "Slice_" + (x) );
//			saveImage(temp, temp.getImageName(), dirName );
//			System.err.println( "... done" );
//		}
//	}
	
	public void setResolutions(float rX, float rY, float rZ)
	{
		resolutions[0] = rX;
		resolutions[1] = rY;
		resolutions[2] = rZ;
//		zScale = rZ/rX;
	}
	
	public float[] getResolutions()
	{
		return resolutions;
	}
	
	public float getXRes()
	{
		return resolutions[0];
	}
	
	public float getYRes()
	{
		return resolutions[1];
	}
	
	public float getZRes()
	{
		return resolutions[2];
	}
	
//	public float getZScale()
//	{
//		return zScale;
//	}
	
	public boolean checkBounds( int x, int y, int z )
	{
		x -= extents3D[0]/2;
		y -= extents3D[1]/2;

		x /= resolutions[0];
		y /= resolutions[1];

		x += extents3D[0]/2;
		y += extents3D[1]/2;
				
		if ( (x < 0) || (y < 0) || (z < 0)  ||
			 (x >= extents2D[0]) || (y >= extents2D[1]) )
			return false;
		
		if ( (Math.ceil(z / resolutions[2]) + sliceOffset) >= extents3D[2] )
			return false;
		
		return true;
	}

	private void saveImage(final ModelImage image, String name, String outputDirectory)
	{
		File voiFileDir = new File(outputDirectory);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiFileDir = null;
		File file = new File(outputDirectory + name);
		if (file.exists()) {
			file.delete();
		}
		ModelImage.saveImage(image, name + ".tif", outputDirectory, false);
//		System.err.println( "saveImage " + voiDir + " " + imageName + ".tif" );
		voiFileDir = null;
	}
	
	private void initFileTable( boolean readFromDisk )
	{
		fileTable = new HashMap<Integer,String[]>();
		if ( readFromDisk )
		{
			//		System.err.println( directory );
			File imageFile = new File(directory + name);
			if ( imageFile.exists() )
			{
				if ( imageFile.isDirectory() )
				{
					int min = Integer.MAX_VALUE;
					String[] list = imageFile.list();
					for ( int i = 0; i < list.length; i++ )
					{
						int value = ModelImageLargeFormat.getIndex(list[i]);
						fileTable.put(value, new String[]{ JDialogBase.makeImageName( list[i], "" ), directory + name + File.separator});
						if ( value < min )
						{
							min = value;
						}
					}
					if ( min > 0 )
					{
						sliceOffset = min;
					}
				}
			}
		}
		else
		{
			for ( int i = 0; i < extents3D[2]; i++ )
			{
				fileTable.put(i, new String[]{ name + "_" + i, directory + name + File.separator} );
			}
		}
//		System.err.println( sliceOffset );
	}
	
	private ModelImage readFromDisk( int index )
	{
		ModelImage image = null;
		String[] fileInfo = fileTable.get(index);
		if ( fileInfo != null )
		{
			File file = new File(fileInfo[1] + fileInfo[0] + ".png");
			if (file.exists()) {
				if ( fileIO == null )
				{
					fileIO = new FileIO();
					fileIO.setQuiet(true);
					fileIO.setSuppressProgressBar(true);
				}
				image = fileIO.readImage(fileInfo[0] + ".png", fileInfo[1], false, null);
				//				fileIO.dispose();
//				System.err.println( "found file " + fileInfo[1] + fileInfo[0] + ".png" + " " + image.getTypeString() );
			}
			else
			{
				file = new File(fileInfo[1] + fileInfo[0] + ".tif");
				if (file.exists()) {
					if ( fileIO == null )
					{
						fileIO = new FileIO();
						fileIO.setQuiet(true);
						fileIO.setSuppressProgressBar(true);
					}
					//				System.err.println( "found file " + fileInfo[1] + fileInfo[0] + ".itf" );
					image = fileIO.readImage(fileInfo[0] + ".tif", fileInfo[1], false, null);
					//				fileIO.dispose();
				}
				else
				{
					file = new File(fileInfo[1] + fileInfo[0] + ".xml");
					if (file.exists()) {
						if ( fileIO == null )
						{
							fileIO = new FileIO();
							fileIO.setQuiet(true);
							fileIO.setSuppressProgressBar(true);
						}
						//				System.err.println( "found file " + fileInfo[1] + fileInfo[0] + ".itf" );
						image = fileIO.readImage(fileInfo[0] + ".xml", fileInfo[1], false, null);
						//				fileIO.dispose();
					}
					else {
						MipavUtil.displayError( "File not found " + fileInfo[1] + " " + fileInfo[0] );
					}
				}
			}
			file = null;
		}
		return image;
	}
		
	private ModelImage updateSliceTable(int index)
	{
		index += sliceOffset;
		ModelImage slice = sliceTable.get(index);
		if ( slice != null )
		{
			lruList.remove(slice);
			lruList.add(0, slice);
			return slice;
		}
		
		// Check memory and delete LRU slice:
        long memoryInUse = MipavUtil.getUsedHeapMemory() / 1048576;
        long totalMemory = MipavUtil.getMaxHeapMemory() / 1048576;

        if ( ((double) memoryInUse / (double) totalMemory) > 0.8)
        {
        	int numRemove = (int)Math.max( 1, sliceTable.size()*.20 );
        	
//            System.err.println( name + " " + index + " " + numRemove + " " + sliceTable.size() + "    " + memoryInUse + " " + totalMemory + " " + ((double) memoryInUse / (double) totalMemory) );
        	for ( int i = 0; i < numRemove; i++ )
        	{
        		ModelImage removeSlice = lruList.lastElement();
        		Iterator<Integer> iterator = sliceTable.keySet().iterator();
        		while ( iterator.hasNext() )
        		{
        			Integer key = iterator.next();
        			if ( sliceTable.get(key) == removeSlice )
        			{
        				sliceTable.remove(key);
        				lruList.remove(removeSlice);
        				if ( modified[key] )
        				{
        					String[] fileInfo = fileTable.get(key);
        					//        				System.err.println( "updateSliceTable " + key + " " + fileInfo[0] + " " + fileInfo[1] );
        					removeSlice.setImageName( fileInfo[0] );
        					saveImage( removeSlice, fileInfo[0], fileInfo[1] );
        					modified[key] = false;
        					onDisk[key] = true;
        				}
        				//        			System.err.println( "updateSliceTable removing from table: " + key );
        				break;
        			}
        		}
        		removeSlice.disposeLocal(false);
        		removeSlice = null;
        	}
        	System.gc();
        }
		
		// add new slice:
		if ( onDisk[index] )
		{
			try {
				slice = readFromDisk(index);
			} catch ( java.lang.OutOfMemoryError e )
			{
				int numRemove = (int)Math.max( 1, sliceTable.size()*.20 );
				for ( int i = 0; i < numRemove; i++ )
				{
					ModelImage removeSlice = lruList.lastElement();
					Iterator<Integer> iterator = sliceTable.keySet().iterator();
					while ( iterator.hasNext() )
					{
						Integer key = iterator.next();
						if ( sliceTable.get(key) == removeSlice )
						{
							sliceTable.remove(key);
							lruList.remove(removeSlice);
							if ( modified[key] )
							{
								String[] fileInfo = fileTable.get(key);
								//        				System.err.println( "updateSliceTable " + key + " " + fileInfo[0] + " " + fileInfo[1] );
								removeSlice.setImageName( fileInfo[0] );
								saveImage( removeSlice, fileInfo[0], fileInfo[1] );
								modified[key] = false;
								onDisk[key] = true;
							}
							//        			System.err.println( "updateSliceTable removing from table: " + key );
							break;
						}
					}
					removeSlice.disposeLocal(false);
					removeSlice = null;
				}
				System.gc();

				slice = readFromDisk(index);
			}
		}
		if ( slice == null )
		{
			if ( dataType == INT )
			{
//				System.err.println( sliceTable.size() + " " + index );
				slice = new ModelImage(ModelStorageBase.INTEGER, extents2D, "Slice_" + index );
			}
			else if ( dataType == ARGB )
			{
				slice = new ModelImage(ModelStorageBase.ARGB, extents2D, "Slice_" + index );
			}
			else if ( dataType == ARGB_FLOAT )
			{
				slice = new ModelImage(ModelStorageBase.ARGB_FLOAT, extents2D, "Slice_" + index );
			}
		}
		sliceTable.put(index, slice);
		lruList.add(0, slice);
		modified[index] = false;
		return slice;
	}
}
