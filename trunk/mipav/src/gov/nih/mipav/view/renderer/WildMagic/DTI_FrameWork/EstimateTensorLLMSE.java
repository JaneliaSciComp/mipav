package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;
import Jama.Matrix;

public class EstimateTensorLLMSE {
	private static boolean detailedDebugging = false;

	public static ModelImage estimate( ModelImage image, boolean usePartialEstimates )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		int dimDW = image.getExtents().length > 3 ? image.getExtents()[3] : 1;
		
		DTIParameters dtiparams = image.getDTIParameters();
		dimDW = Math.min( dimDW, dtiparams.getNumVolumes() );
		
		int tempDimDW = dimDW;
		for ( int dw = 1; dw < dimDW; dw++ )
		{
			if ( (dtiparams.getGradients()[dw][0] == 0) &&
					(dtiparams.getGradients()[dw][1] == 0) &&
					(dtiparams.getGradients()[dw][2] == 0) )
			{
				tempDimDW--;	
			}
		}
		
		float[][][][] DWdata = new float[dimX][dimY][dimZ][tempDimDW];
		float[] bvalues = new float[tempDimDW];
		float[][] grads = new float[tempDimDW][3];
		byte[][][] mask = null;
		if ( (image.getMask() != null) && image.useMask() )
		{
			mask = new byte[dimX][dimY][dimZ];
		}
		
		int index = 0;
		for ( int i = 0; i < dimDW; i++ )
		{
			if ( (i > 0) &&
					(dtiparams.getGradients()[i][0] == 0) &&
					(dtiparams.getGradients()[i][1] == 0) &&
					(dtiparams.getGradients()[i][2] == 0) )
			{
				continue;
			}
			bvalues[index] = dtiparams.getbValues()[index];
			grads[index][0] = dtiparams.getGradients()[index][0];
			grads[index][1] = dtiparams.getGradients()[index][1];
			grads[index][2] = dtiparams.getGradients()[index][2];
			
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						DWdata[x][y][z][index] = image.getFloat( x, y, z, index );
						if ( (index == 0) && (mask != null) )
						{
							if ( image.getMask().get( z * dimY * dimX + y * dimX + x ) )
							{
								mask[x][y][z] = 1;
							}
							else
							{
								mask[x][y][z] = 0;
							}		
						}
					}
				}
			}
			index++;
		}
		
		float[][][][] tensors = estimate( DWdata, bvalues, grads, mask, usePartialEstimates );
		String name = JDialogBase.makeImageName(image.getImageName(), "_tensor");
		ModelImage tensorImage = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ,6}, name );
		JDialogBase.updateFileInfo( image, tensorImage );
		for ( int dw = 0; dw < 6; dw++ )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						tensorImage.set( x, y, z, dw, tensors[x][y][z][dw] );
					}
				}
			}
		}
		tensorImage.calcMinMax();
		new ViewJFrameImage( tensorImage );
		return tensorImage;
	}
	
	
	/****************************************************
	 * estimate 
	 * 		Use LLMSE to estimate tensors from DW imaging data
	 * 		on a voxel-wise basis. 
	 * 
	 * Inputs: 
	 * 		DWdata - 4D matrix indexed by [x][y][z][dw] where
	 * 				x,y,z are spatial location and dw is diffusion
	 * 				sensitization.
	 * 		bvalues - a list [dw] of b-values indicating the diffusion 
	 * 				sensitization for each volume in the 4th 
	 * 				dimension of the input data 
	 * 					>0 for DW=yes (in s/mm2)
	 * 					=0 for reference images
	 * 					<0 for volumes to ignore
	 * 		grads - a set of unit vectors [dw][3] indicating the diffusion
	 * 				weighting for each of the input volumes. Vectors corresponding
	 * 				to reference or ignored volumes are ignored. 
	 * 		mask - a boolean mask [x][y][z] indicating which voxels in the volume
	 * 				show be used for tensor estimation. If mask is null, then all 
	 * 				voxels are processed. 
	 * 		usePartialEstimates - boolean
	 * 					true - if any DW data are infinity, NaN, negative or zero (i.e., impossible values)
	 * 						then these data points will be ignored and a tensor will
	 * 						be computed with the remaining data (provided that at 
	 * 						least 6 valid DW values and a reference value are 
	 * 						available)
	 * 					false - any voxels with any missing DW data are ignored.
	 * 
	 * Output: 	
	 * 		return value - set of tensors [x][y][z][6], where the tensor coefficients 
	 * 				are ordered by upper triangular convention, i.e., xx,xy,xz,yy,yz,zz
	 * 				Units are in mm2/s. 
	 * 	
	 *  Notes: Voxels for which no tensors are computer are reported as all NaN.
	 *  
	 *   Last updated: 8/13/2008 Bennett Landman.
	 ****************************************************/
	public static float[][][][] estimate(float [][][][]DWdata, 
			float []bvalues, float [][]grads, byte [][][]mask, boolean usePartialEstimates) {

		/****************************************************
		 * Step 1: Validate Input Arguments 
		 ****************************************************/
		int bvalList[] = null;
		int gradList[] = null;
		int Nvols = DWdata[0][0][0].length;
		if(Nvols!=bvalues.length)
			throw new RuntimeException("EstimateTensorLLMSE: Number of volumes does not match number of bvalues.");
		if(Nvols!=grads.length)
			throw new RuntimeException("EstimateTensorLLMSE: Number of volumes does not match number of gradient directions.");

		if(mask==null) {
			mask = new byte[DWdata.length][DWdata[0].length][DWdata[0][0].length];
			for(int i=0;i<DWdata.length;i++) {

				for(int j=0;j<DWdata[0].length;j++) {

					for(int k=0;k<DWdata[0][0].length;k++)
						mask[i][j][k]=1;
				}
			}
		} 
		if(mask.length!=DWdata.length)
			throw new RuntimeException("EstimateTensorLLMSE: Mask does not match data in dimension: 0.");
		if(mask[0].length!=DWdata[0].length)
			throw new RuntimeException("EstimateTensorLLMSE: Mask does not match data in dimension: 1.");
		if(mask[0][0].length!=DWdata[0][0].length)
			throw new RuntimeException("EstimateTensorLLMSE: Mask does not match data in dimension: 2.");


		int Ngrad = 0;
		int Nb0 = 0; 
		for(int i=0;i<bvalues.length;i++) {
			if(bvalues[i]==0)
				Nb0++;
			if(bvalues[i]>0 && grads[i][0]<90)
				Ngrad++;
		}

		if(Nb0==0)
			throw new RuntimeException("EstimateTensorLLMSE: No reference images specified.");

		if(Ngrad<6)
			throw new RuntimeException("EstimateTensorLLMSE: Less than 6 diffusion weighted volumes specified.");

		/****************************************************
		 * Step 2: Index b0 and DW images and normalize DW directions
		 ****************************************************/

		bvalList = new int[Nb0];
		gradList = new int[Ngrad];
		Ngrad = 0;
		Nb0 = 0; 
		for(int i=0;i<bvalues.length;i++) {
			if(bvalues[i]==0) {
				bvalList[Nb0]=i;
				Nb0++;
			}

			if(bvalues[i]>0 && grads[i][0]<90) {
				gradList[Ngrad]=i;
				float norm = (float)Math.sqrt(grads[i][0]*grads[i][0]+
						grads[i][1]*grads[i][1]+
						grads[i][2]*grads[i][2]);
				if(norm==0)
					throw new RuntimeException("EstimateTensorLLMSE: Invalid DW Direction "+i+": ("+grads[i][0]+","+grads[i][1]+","+grads[i][2]+");");
				grads[i][0]/=norm;
				grads[i][1]/=norm;
				grads[i][2]/=norm;


				Ngrad++;
			}
		}

		/****************************************************
		 * Step 3: Build the imaging and inversion matrix 
		 ****************************************************/
		Matrix reconMatrix=null;
		Matrix imagMatrix = new Matrix(gradList.length,6);
		for(int ii=0;ii<gradList.length;ii++) {
			//xx
			imagMatrix.set(ii,0,bvalues[gradList[ii]]*grads[gradList[ii]][0]*grads[gradList[ii]][0]);
			//			2xy
			imagMatrix.set(ii,1,bvalues[gradList[ii]]*grads[gradList[ii]][0]*grads[gradList[ii]][1]*2);
			//			2xz
			imagMatrix.set(ii,2,bvalues[gradList[ii]]*grads[gradList[ii]][0]*grads[gradList[ii]][2]*2);
			//			yy
			imagMatrix.set(ii,3,bvalues[gradList[ii]]*grads[gradList[ii]][1]*grads[gradList[ii]][1]);
			//			2yz
			imagMatrix.set(ii,4,bvalues[gradList[ii]]*grads[gradList[ii]][1]*grads[gradList[ii]][2]*2);
			//			zz
			imagMatrix.set(ii,5,bvalues[gradList[ii]]*grads[gradList[ii]][2]*grads[gradList[ii]][2]);
		}
		reconMatrix = imagMatrix.inverse(); // (actually, pseudoinverse)

		if(imagMatrix.rank()<6) {
			System.out.println("jist.plugins"+"\t"+"EstimateTensorLLMSE : **********WARNING**********");
			System.out.println("jist.plugins"+"\t"+"EstimateTensorLLMSE : Gradient table of rank < 6");
			System.out.println("jist.plugins"+"\t"+"EstimateTensorLLMSE : ***************************");
		}


		/*****)***********************************************
		 * Step 4: Allocate output matrix 
		 ****************************************************/
		float tensors[][][][] = new float[DWdata.length][DWdata[0].length][DWdata[0][0].length][6];

		/****************************************************
		 * Step 5: Loop over all voxels and estimate tensors 
		 ****************************************************/
		float mb0=0;
		int cnt;
		//float []DW = new float[gradList.length];
		Matrix DW = new Matrix(gradList.length,1);
		int ignoreDW;
		Matrix tensorMatrix; 
		Matrix DWsubset=null;
		for(int i=0;i<DWdata.length;i++) {
			for(int j=0;j<DWdata[0].length;j++) {
				for(int k=0;k<DWdata[0][0].length;k++) {
					if(mask[i][j][k]!=0) {
						mb0=0;
						cnt=0;
						for(int ii=0;ii<bvalList.length;ii++) {
							if(!Float.isNaN(DWdata[i][j][k][bvalList[ii]])) {
								mb0+=DWdata[i][j][k][bvalList[ii]];
								cnt++;
							}
						}						
						mb0/=cnt;
						if(mb0<=0) {//can't do anything here - report NaN tnso
							for(int ii=0;ii<6;ii++) {
								tensors[i][j][k][ii] = Float.NaN;
							}
						} else {

							ignoreDW=0;
							for(int ii=0;ii<gradList.length;ii++) {
								double val=-Math.log(DWdata[i][j][k][gradList[ii]]/mb0);
								if(Double.isNaN(val)  || Double.isInfinite(val))
									ignoreDW++;
								DW.set(ii, 0, val);
							}
							if(ignoreDW!=0) {
								// BEGIN BUILD A NEW RECON MATRIX													
								if(((!usePartialEstimates)) || (gradList.length-ignoreDW<6)) {
									// no full-rank solutions available
									for(int ii=0;ii<6;ii++) {
										tensors[i][j][k][ii] = Float.NaN;
									}
								} else {
									imagMatrix = new Matrix(gradList.length-ignoreDW,6);
									DWsubset = new Matrix(gradList.length-ignoreDW,1);
									int jj=0;
									for(int ii=0;ii<gradList.length;ii++) {
										if(!(Double.isNaN(DW.get(ii,0)) || Double.isInfinite(DW.get(ii,0)))) {
											DWsubset.set(jj,0,DW.get(ii,0));
											try {
												//xx
												imagMatrix.set(jj,0,bvalues[gradList[ii]]*grads[gradList[ii]][0]*grads[gradList[ii]][0]);
												//												2xy
												imagMatrix.set(jj,1,bvalues[gradList[ii]]*grads[gradList[ii]][0]*grads[gradList[ii]][1]*2);
												//												2xz
												imagMatrix.set(jj,2,bvalues[gradList[ii]]*grads[gradList[ii]][0]*grads[gradList[ii]][2]*2);
												//												yy
												imagMatrix.set(jj,3,bvalues[gradList[ii]]*grads[gradList[ii]][1]*grads[gradList[ii]][1]);
												//												2yz
												imagMatrix.set(jj,4,bvalues[gradList[ii]]*grads[gradList[ii]][1]*grads[gradList[ii]][2]*2);
												//												zz
												imagMatrix.set(jj,5,bvalues[gradList[ii]]*grads[gradList[ii]][2]*grads[gradList[ii]][2]);

												jj++;
											} catch(Exception e){
												System.out.println("jist.plugins"+"\t"+e);
											}
										}
									}


									// END BUILD A NEW RECON MATRIX
									try {
										tensorMatrix = imagMatrix.inverse().times(DWsubset);
										for(int ii=0;ii<6;ii++) {
											tensors[i][j][k][ii] = (float)tensorMatrix.get(ii, 0);
										}
									} catch(Exception e) {
										// no full-rank solutions available
										for(int ii=0;ii<6;ii++) {
											tensors[i][j][k][ii] = Float.NaN;
										}
									}
								}
							} else {
								tensorMatrix = reconMatrix.times(DW);
								for(int ii=0;ii<6;ii++) {
									tensors[i][j][k][ii] = (float)tensorMatrix.get(ii, 0);
								}
							}


						}
					}
				}
			}
		}


		return tensors;
	}


	public static boolean test() {
		/****************************************************
		 * Perform 1000 tests. 
		 ****************************************************/
		return test(1000);
	}

	public static boolean test(int count) {
		/****************************************************
		 * Perform testing using randomly generated tensors. 
		 ****************************************************/
		System.out.println("jist.plugins"+"\t"+"EstimateTensorLLMSE: Unit Testing. Generateing and estimate random tensors");
		int failed=0;
		Matrix tensor = new Matrix(6,1);
		java.util.Random r= new java.util.Random();
		float [][][][] DWdata = new float[1][1][1][17];
		float []bvalues = new float[17];
		float [][]grads = new float[17][3];

		for(int i=0;i<count;i++) {
			for(int j=0;j<6;j++)
				tensor.set(j,0,r.nextFloat()/100);

			if(detailedDebugging) {
				System.out.println("jist.plugins"+"\t"+"T=[");
				for(int k=0;k<6;k++) {
					System.out.println("jist.plugins"+"\t"+tensor.get(k,0));
				}
				System.out.println("jist.plugins"+"\t"+"]; %T");
			}

			for(int j=0;j<17;j++) {
				if(j<2)
					bvalues[j]=0;
				else { 
					bvalues[j]=1000.f*r.nextFloat()+100;
					float norm =0;
					for(int k=0;k<3;k++) {
						grads[j][k] = r.nextFloat()-0.5f;
						norm+=grads[j][k]*grads[j][k];
					}
					norm=(float)Math.sqrt(norm);
					for(int k=0;k<3;k++)
						grads[j][k]/=norm;
				}				
			}
			if(detailedDebugging) {			
				System.out.println("jist.plugins"+"\t"+"b=[");
				for(int k=0;k<17;k++) {
					System.out.println("jist.plugins"+"\t"+bvalues[k]);
				}
				System.out.println("jist.plugins"+"\t"+"]; %b");

				System.out.println("jist.plugins"+"\t"+"g=[");
				for(int k=0;k<17;k++) {
					System.out.println("jist.plugins"+"\t"+grads[k][0]+" "+grads[k][1]+" "+grads[k][2]+" ");
				}
				System.out.println("jist.plugins"+"\t"+"]; %g");
			}
			Matrix imagMatrix = new Matrix(17,6);
			for(int ii=0;ii<17;ii++) {
				//xx
				imagMatrix.set(ii,0,-bvalues[ii]*grads[ii][0]*grads[ii][0]);
				//				2xy
				imagMatrix.set(ii,1,-bvalues[ii]*grads[ii][0]*grads[ii][1]*2);
				//				2xz
				imagMatrix.set(ii,2,-bvalues[ii]*grads[ii][0]*grads[ii][2]*2);
				//				yy
				imagMatrix.set(ii,3,-bvalues[ii]*grads[ii][1]*grads[ii][1]);
				//				2yz
				imagMatrix.set(ii,4,-bvalues[ii]*grads[ii][1]*grads[ii][2]*2);
				//				zz
				imagMatrix.set(ii,5,-bvalues[ii]*grads[ii][2]*grads[ii][2]);
			}



			Matrix num = imagMatrix.times(tensor);			
			for(int j=0;j<17;j++) 
				DWdata[0][0][0][j]=(float)Math.exp(num.get(j,0));

			if(detailedDebugging) {
				System.out.println("jist.plugins"+"\t"+"E=[");
				for(int k=0;k<17;k++) {
					System.out.println("jist.plugins"+"\t"+DWdata[0][0][0][k]);
				}
				System.out.println("jist.plugins"+"\t"+"]; %E");			
				System.out.flush();
			}
			float [][][][]result = estimate(DWdata, 
					bvalues, grads, null,true);

			double err = 0; 
			for(int j=0;j<6;j++)
				err+=Math.abs(result[0][0][0][j]-tensor.get(j,0));

			System.out.println("jist.plugins"+"\t"+"Abs Error: "+err);

			if(err>1e-7) {
				failed++;
			}
			//			float [][][][]DWdata, 
			//			float []bvalues, float [][]grads, boolean [][][]mask
		}
		if(failed==0) {
			System.out.println("jist.plugins"+"\t"+"Random Testing Passed.");
			return true;
		} else {
			System.out.println("jist.plugins"+"\t"+"**********Random Testing Failed.*************");
			return false;
		}
	}

	public static void main(String args[]) {
		/****************************************************
		 * Run the testing script by default. 
		 ****************************************************/
		test();
	}
}
