package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.io.File;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class WormSegmentationKMeans extends WormSegmentation
{
	public WormSegmentationKMeans() {}
	
	public static Vector<Vector3f> seamCellSegmentation( ModelImage image )
	{
		ModelImage temp = (ModelImage)image.clone();
		temp.calcMinMax();
		float maxValue = (float) (1.0 * image.getMax());
		float minValue = (float) (0.5 * image.getMax());
		
		Vector<Vector3f> seam1 = LatticeModel.segmentAll1(image, temp, minValue, maxValue, 20, 1);
		maxValue = minValue;
		minValue = (float) (0.4 * image.getMax());
		Vector<Vector3f> seam2 = LatticeModel.segmentAll1(image, temp, minValue, maxValue, 20, .5f);
		maxValue = minValue;
		minValue = (float) (0.3 * image.getMax());
		Vector<Vector3f> seam3 = LatticeModel.segmentAll1(image, temp, minValue, maxValue, 20, .25f);
		maxValue = minValue;
		minValue = (float) (0.2 * image.getMax());
		Vector<Vector3f> seam4 = LatticeModel.segmentAll1(image, temp, minValue, maxValue, 20, .75f);
		maxValue = minValue;
		minValue = (float) (0.15 * image.getMax());
		Vector<Vector3f> seam5 = LatticeModel.segmentAll1(image, temp, minValue, maxValue, 20, .9f);

		Vector<Vector3f> seamCells = new Vector<Vector3f>();
		Vector<Vector3f> tempSeamCells = new Vector<Vector3f>();
		if ( seam1 != null )		tempSeamCells.addAll(seam1);
		if ( seam2 != null )		tempSeamCells.addAll(seam2);
		if ( seam3 != null )		tempSeamCells.addAll(seam3);
		if ( seam4 != null )		tempSeamCells.addAll(seam4);
		if ( seam5 != null )		tempSeamCells.addAll(seam5);

		Vector3f negCenter = new Vector3f(-1,-1,-1);
//		for ( int i = 0; i < tempSeamCells.size(); i++ )
//		{
//			if ( !tempSeamCells.elementAt(i).equals(negCenter) )
//			{
//				Vector3f newCenter = new Vector3f(tempSeamCells.elementAt(i));
//				int count = 1;
//				for ( int j = i+1; j < tempSeamCells.size(); j++ )
//				{
//					if ( !tempSeamCells.elementAt(j).equals(negCenter) )
//					{
//						if ( tempSeamCells.elementAt(i).distance(tempSeamCells.elementAt(j)) < 4 )
//						{
//							newCenter.add(tempSeamCells.elementAt(j));
//							tempSeamCells.elementAt(j).copy(negCenter);
//							count++;
//						}
//					}
//				}
//				if ( count > 1 )
//				{
//					newCenter.scale(1f/(float)count);
//					tempSeamCells.elementAt(i).copy(newCenter);
//				}
//				else
//				{
//					tempSeamCells.elementAt(i).copy(negCenter);
//				}
//			}
//		}
		WormSegmentation.reduceDuplicates(image, tempSeamCells, true);
		
		for ( int i = 0; i < tempSeamCells.size(); i++ )
		{
			// System.err.println( i + "     " + potentialClusters[i] );
			if ( !tempSeamCells.elementAt(i).equals(negCenter) )
			{
				seamCells.add(tempSeamCells.elementAt(i));
			}
		}
		temp.disposeLocal();
		temp = null;
		return seamCells;
	}
}
