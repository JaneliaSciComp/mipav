import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;


public class PlugInAlgorithmImageStitchingGUI extends AlgorithmBase {

	private ArrayList<ModelImage> images;
	
	private ArrayList<Vector3f> relShift;
	
	private ArrayList<Vector3f> absShift;
	
	private String imDir;
	
	private String imName;
	
	public PlugInAlgorithmImageStitchingGUI(ArrayList<ModelImage> imageList, ArrayList<Vector3f> relTrans){
		images = imageList;
		relShift = relTrans;
		absShift = new ArrayList<Vector3f>();
	}
	
	@Override
	public void runAlgorithm() {

		if(images.get(0).getNDims() == 2)
			preview2D();
		else
			addImagesSlice();
	}
	
	public void setNames(String dir, String name){
		imDir = dir;
		imName = name;
	}
	
	private void addImagesSlice(){
			
		int width = images.get(0).getWidth(0);
		int height = images.get(0).getHeight(0);
		int depth = images.get(0).getExtents()[2];
		int length = width*height;
	
		Vector3f shift, vecB;
		shift = relShift.get(0);
		int maxShiftX = 0;
		int maxShiftY = 0;
		int minShiftX = 0;
		int minShiftY = 0;
		
		absShift.add(new Vector3f());
		for(int i=1;i<images.size();i++){
			vecB = relShift.get(i);
			shift = Vector3f.add(shift, vecB);
			if(shift.X > maxShiftX)
				maxShiftX = (int) shift.X;
			else if(shift.X < minShiftX)
				minShiftX = (int) shift.X;
			if(shift.Y > maxShiftY)
				maxShiftY = (int) shift.Y;
			else if(shift.Y < minShiftY)
				minShiftY = (int) shift.Y;
			absShift.add(shift);
		}
	
		int stitchWidth = maxShiftX - minShiftX + width;
		int stitchHeight = maxShiftY - minShiftY + height;
	
		for(int i=0;i<images.size();i++){
			shift = absShift.get(i);
			shift.X -= minShiftX;
			shift.Y -= minShiftY;
		}
	
		int[] newExtents;
		newExtents = new int[]{stitchWidth, stitchHeight};
	
		destImage = new ModelImage(images.get(0).getType(), newExtents, "Stitched Image");
	
		ModelImage imageA, imageB;

		Vector3f absoluteShift;
		Vector3f shiftPt;
		
		int subwidth, subheight;
		int offsetXa, offsetYa;
		int offsetXb, offsetYb;
		
		int x, y, ptX, ptY;
		int row, rowb, stitchedRow, stitchedCol;
		int nx, ny, ind;
		x = 0;
		y = 0;
		
		int[] imBufferA = new int[length];
		int[] imBufferB;
		
		try{
			for(int k=0;k<depth;k++){
				
				fireProgressStateChanged((int)(100*k/depth), null,
						"Saving slice " + k + " of " + depth);
				
				imageA = images.get(0);	
				imageA.exportData(k*length,length, imBufferA);
				int[] stitchBuffer = new int[stitchWidth*stitchHeight];
				Arrays.fill(stitchBuffer, -1);
				
				//System.out.println("Saving slice " + k);
				
				for(int i=1;i<images.size();i++){
					imBufferB = new int[length];
					imageB = images.get(i);
					shiftPt = relShift.get(i);
					absoluteShift = absShift.get(i-1);
					imageB.exportData(k*length,length, imBufferB);
					
					x = (int) absoluteShift.X;
					y = (int) absoluteShift.Y;
					ptX = (int) shiftPt.X;
					ptY = (int) shiftPt.Y;
					
					offsetXa = Math.max(0, ptX);
					offsetXb = Math.max(0, -ptX);
					subwidth = width - Math.abs(ptX);
					offsetYa = Math.max(0, ptY);
					offsetYb = Math.max(0, -ptY);
					subheight = height - Math.abs(ptY);
					
					//System.out.println("Writing overlap region");
					
					for(int n=0;n<subheight;n++){
						row = (n + offsetYa)*width;
						rowb = (n + offsetYb)*width;
						stitchedRow = (n+offsetYa+y)*stitchWidth;
						for(int m=0;m<subwidth;m++){
		
							stitchedCol = x + offsetXa + m;
							stitchBuffer[stitchedCol + stitchedRow] =
									//imBufferA[m + offsetXa + row];
									Math.max(imBufferA[m + offsetXa + row],
											imBufferB[m + offsetXb + rowb]);
							/*stitchBuffer[stitchedCol + stitchedRow] += 
									weight * imBuffer[m + offsetXa + row];*/
						}
					}
					
					//System.out.println("Writing non-overlap regions");
					
					for(int j=0;j<length;j++){
						nx = j%width;
						ny = j/width;
						ptX = x + nx;
						ptY = y + ny;
						ind = ptX + ptY*stitchWidth;
						if(stitchBuffer[ind] == -1){
							stitchBuffer[ind] = imBufferA[nx + ny*width];
						}
					}
					
					imBufferA = imBufferB;
						
				}
				
				//System.out.println("Writing final image");
				absoluteShift = absShift.get(absShift.size()-1);
				x = (int) absoluteShift.X;
				y = (int) absoluteShift.Y;
				
				for(int j=0;j<length;j++){
					nx = j%width;
					ny = j/width;
					ptX = x + nx;
					ptY = y + ny;
					ind = ptX + ptY*stitchWidth;
					if(stitchBuffer[ind] == -1){
						stitchBuffer[ind] = imBufferA[nx + ny*width];
					}
				}
				
				for(int i=0;i<stitchWidth*stitchHeight;i++){
					if(stitchBuffer[i] == -1)
						stitchBuffer[i] = 0;
				}
				
				/*try{
					Thread.sleep(500);
				}catch(InterruptedException e){
					
				}*/
				
				destImage.importData(0, stitchBuffer, true);
				destImage.saveImage(imDir + File.separator, imName + "_" + String.format("%02d", k), FileUtility.TIFF, false, false);
				
			}
			
			fireProgressStateChanged(100);
			setCompleted(true);
		}catch(IOException e){
			e.printStackTrace();
		}
	}
	
	private void preview2D(){
		int width = images.get(0).getWidth(0);
		int height = images.get(0).getHeight(0);
		int length = width*height;

		Vector3f shift, vecB;
		shift = relShift.get(0);
		int maxShiftX = 0;
		int maxShiftY = 0;
		int minShiftX = 0;
		int minShiftY = 0;
		absShift.add(new Vector3f());
		for(int i=1;i<images.size();i++){
			vecB = relShift.get(i);
			shift = Vector3f.add(shift, vecB);
			if(shift.X > maxShiftX)
				maxShiftX = (int) shift.X;
			else if(shift.X < minShiftX)
				minShiftX = (int) shift.X;
			if(shift.Y > maxShiftY)
				maxShiftY = (int) shift.Y;
			else if(shift.Y < minShiftY)
				minShiftY = (int) shift.Y;
			absShift.add(shift);
		}

		int stitchWidth = maxShiftX - minShiftX + width;
		int stitchHeight = maxShiftY - minShiftY + height;

		for(int i=0;i<images.size();i++){
			shift = absShift.get(i);
			shift.X -= minShiftX;
			shift.Y -= minShiftY;
		}

		int[] newExtents;
		newExtents = new int[]{stitchWidth, stitchHeight};

		destImage = new ModelImage(images.get(0).getType(), newExtents, "Stitched Image");

		ModelImage imageA, imageB;

		Vector3f absoluteShift;
		Vector3f shiftPt;

		int subwidth, subheight;
		int offsetXa, offsetYa;
		int offsetXb, offsetYb;

		int x, y, ptX, ptY;
		int row, rowb, stitchedRow, stitchedCol;
		int nx, ny, ind;
		x = 0;
		y = 0;

		int[] imBufferA = new int[length];
		int[] imBufferB;

		imageA = images.get(0);	
		try {
			imageA.exportData(0, length, imBufferA);

			int[] stitchBuffer = new int[stitchWidth*stitchHeight];
			Arrays.fill(stitchBuffer, -1);

			for(int i=1;i<images.size();i++){
				imBufferB = new int[length];
				imageB = images.get(i);
				shiftPt = relShift.get(i);
				absoluteShift = absShift.get(i-1);
				imageB.exportData(0, length, imBufferB);

				x = (int) absoluteShift.X;
				y = (int) absoluteShift.Y;
				ptX = (int) shiftPt.X;
				ptY = (int) shiftPt.Y;

				offsetXa = Math.max(0, ptX);
				offsetXb = Math.max(0, -ptX);
				subwidth = width - Math.abs(ptX);
				offsetYa = Math.max(0, ptY);
				offsetYb = Math.max(0, -ptY);
				subheight = height - Math.abs(ptY);

				//System.out.println("Writing overlap region");

				for(int n=0;n<subheight;n++){
					row = (n + offsetYa)*width;
					rowb = (n + offsetYb)*width;
					stitchedRow = (n+offsetYa+y)*stitchWidth;
					for(int m=0;m<subwidth;m++){

						stitchedCol = x + offsetXa + m;
						stitchBuffer[stitchedCol + stitchedRow] =
								//imBufferA[m + offsetXa + row];
								Math.max(imBufferA[m + offsetXa + row],
										imBufferB[m + offsetXb + rowb]);
						/*stitchBuffer[stitchedCol + stitchedRow] += 
								weight * imBuffer[m + offsetXa + row];*/
					}
				}

				//System.out.println("Writing non-overlap regions");

				for(int j=0;j<length;j++){
					nx = j%width;
					ny = j/width;
					ptX = x + nx;
					ptY = y + ny;
					ind = ptX + ptY*stitchWidth;
					if(stitchBuffer[ind] == -1){
						stitchBuffer[ind] = imBufferA[nx + ny*width];
					}
				}

				imBufferA = imBufferB;

			}

			//System.out.println("Writing final image");
			absoluteShift = absShift.get(absShift.size()-1);
			x = (int) absoluteShift.X;
			y = (int) absoluteShift.Y;

			for(int j=0;j<length;j++){
				nx = j%width;
				ny = j/width;
				ptX = x + nx;
				ptY = y + ny;
				ind = ptX + ptY*stitchWidth;
				if(stitchBuffer[ind] == -1){
					stitchBuffer[ind] = imBufferA[nx + ny*width];
				}
			}

			for(int i=0;i<stitchWidth*stitchHeight;i++){
				if(stitchBuffer[i] == -1)
					stitchBuffer[i] = 0;
			}

			destImage.importData(0, stitchBuffer, true);

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
