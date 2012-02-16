/* package PlugInT2;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

public class PlugInAlgorithmT2 extends AlgorithmBase
{
	public static int x;
	public static int y;
	public static int slices;
	public static int TEs;
	public static String fileName;
	public static double[] TEValues;
	
	private boolean	entireImage= true;
	
	public PlugInAlgorithmT2(ModelImage destImg, ModelImage srcImg)
	{
		super(destImg,srcImg);
	}
	
	
	@Override
	public void runAlgorithm() 
	{
		// TODO Auto-generated method stub
	}

	public void finalize()
	{
		destImage = null;
		srcImage = null;
		super.finalize();
	}

	public void run()
	{
		if(srcImage == null || destImage ==null)
		{
			displayError("Source Image is null");
			notifyListeners(this);
			return;
		}

		setStartTime();

		if(destImage != null)
		{
			if(srcImage.getNDims()==2)
				calcStoreInDest2D();
			else if (srcImage.getNDims() > 3)
				calcStoreInDest3D();
		}

		computeElapsedTime();
		notifyListeners(this);

	}

	private void calcStoreInDest2D()
	{
		int length;
		float[] buffer;

		try
		{
			length	= srcImage.getExtents()[0] * srcImage.getExtents()[1];
			buffer	= new float[length];
			srcImage.exportData(0, length, buffer);
		}

		catch(IOException error)
		{
			buffer = null;
			errorCleanUp("Algorithm T2 reports: out of memory", true);
			return;
		}

		int mod = length/100;

		initProgressBar();

		...
		
	}
	
		calcStoreInDest3D();
		// TODO Auto-generated method stub
}
*/