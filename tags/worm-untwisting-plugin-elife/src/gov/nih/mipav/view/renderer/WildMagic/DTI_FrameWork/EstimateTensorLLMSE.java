package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import java.util.Arrays;

import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogBase;
import imaging.B_VectorScheme;
import imaging.DW_Scheme;
import inverters.BallStickInversion;
import inverters.DT_Inversion;
import inverters.LinearDT_Inversion;
import inverters.NonLinearDT_Inversion;
import inverters.RestoreDT_Inversion;
import inverters.ThreeTensorInversion;
import inverters.TwoTensorInversion;
import inverters.WeightedLinearDT_Inversion;
import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.GMatrixd;
import apps.EstimateSNR;

public class EstimateTensorLLMSE {
	private static boolean detailedDebugging = false;

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
			double []bvalues, double [][]grads, byte [][][]mask, boolean usePartialEstimates) {

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
		double bvaluesSort[] = bvalues.clone();
		Arrays.sort(bvaluesSort);
		double bvaluesAbsMax = Math.max(Math.abs(bvaluesSort[0]), Math.abs(bvalues[bvaluesSort.length - 1]));
		double bvaluesThreshold = bvaluesAbsMax/250.0;
		for(int i=0;i<bvalues.length;i++) {
			if(Math.abs(bvalues[i]) < bvaluesThreshold)
				Nb0++;
			if(bvalues[i]>= bvaluesThreshold && grads[i][0]<90)
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
			if(Math.abs(bvalues[i]) < bvaluesThreshold) {
				bvalList[Nb0]=i;
				Nb0++;
			}

			if(bvalues[i]>= bvaluesThreshold && grads[i][0]<90) {
				gradList[Ngrad]=i;
				double norm = Math.sqrt(grads[i][0]*grads[i][0]+
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
	

	public static ModelImage estimate(ModelImage dwiImage, ModelImage mask, GMatrixd matrix, boolean usePartialEstimates) {


        int iDimX =   dwiImage.getExtents().length > 0 ? dwiImage.getExtents()[0] : 1;
        int iDimY =   dwiImage.getExtents().length > 1 ? dwiImage.getExtents()[1] : 1;
        int iSlices = dwiImage.getExtents().length > 2 ? dwiImage.getExtents()[2] : 1; 
        int volumes = dwiImage.getExtents().length > 3 ? dwiImage.getExtents()[3] : 1;   
        
		/****************************************************
		 * Step 3: Build the imaging and inversion matrix 
		 ****************************************************/
		Matrix reconMatrix=null;
		Matrix imagMatrix = new Matrix(volumes-1,6);
		for(int ii=1;ii<matrix.GetRows();ii++) {
			for(int j=0;j<6;j++) {
				imagMatrix.set(ii-1,j,matrix.Get(ii, j));
			}
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
        
		float tensors[][][][] = new float[iDimX][iDimY][iSlices][6];

		/****************************************************
		 * Step 5: Loop over all voxels and estimate tensors 
		 ****************************************************/
		float mb0=0;
		Matrix DW = new Matrix(volumes-1,1);
		int ignoreDW;
		Matrix tensorMatrix; 
		Matrix DWsubset=null;
		for(int i=0;i<iDimX;i++) {
			for(int j=0;j<iDimY;j++) {
				for(int k=0;k<iSlices;k++) {
					if( (mask == null) || ((mask != null) && mask.getBoolean(i,j,k)) ) {
						mb0=0;
						// B0 is in index 0:
						if(!Float.isNaN(dwiImage.getFloat(i, j, k, 0) )) {
							mb0+=dwiImage.getFloat(i, j, k, 0);
						}
						if(mb0<=0) {//can't do anything here - report NaN tnso
							for(int ii=0;ii<6;ii++) {
								tensors[i][j][k][ii] = Float.NaN;
							}
						} else {

							ignoreDW=0;
							// All others are 1-volume index:
							for(int ii=1;ii<volumes;ii++) {
								double val=-Math.log(dwiImage.getFloat(i, j, k, ii)/mb0);
								if(Double.isNaN(val)  || Double.isInfinite(val))
									ignoreDW++;
								DW.set(ii-1, 0, val);
							}
							if(ignoreDW!=0) {
								// BEGIN BUILD A NEW RECON MATRIX													
								if(((!usePartialEstimates)) || ((volumes-1)-ignoreDW<6)) {
									// no full-rank solutions available
									for(int ii=0;ii<6;ii++) {
										tensors[i][j][k][ii] = Float.NaN;
									}
								} else {
									imagMatrix = new Matrix((volumes-1)-ignoreDW,6);
									DWsubset = new Matrix((volumes-1)-ignoreDW,1);
									int jj=0;
									for(int ii=0;ii<(volumes-1);ii++) {
										if(!(Double.isNaN(DW.get(ii,0)) || Double.isInfinite(DW.get(ii,0)))) {
											DWsubset.set(jj,0,DW.get(ii,0));
											try {
												for ( int kk = 0; kk < 6; kk++ )
												{
													imagMatrix.set(jj,kk,matrix.Get(jj, kk));													
												}
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


		String name = JDialogBase.makeImageName(dwiImage.getImageName(), "_tensor");	
		ModelImage tensorImage = makeTensorImage( tensors, null, name );
		JDialogBase.updateFileInfo( dwiImage, tensorImage );
		return tensorImage;
	}
	
	
	/**
	 * Uses LLMSE to estimate tensors from DW imaging data on a voxel-wise basis. 
	 * @param image input DWI images series 4D
	 * @param maskImage input mask image (or null) 3D
	 * @param usePartialEstimates when true if any DW data are infinity, NaN, negative or zero (i.e., impossible values)
	 * 	then these data points will be ignored and a tensor will be computed with the remaining data (provided that at 
	 * 	least 6 valid DW values and a reference value are  available).  When false - any voxels with any missing DW data are ignored.
	 * @return ModelImage with tensor data.
	 */
	public static ModelImage estimate( ModelImage image, ModelImage maskImage, boolean usePartialEstimates )
	{
		DTIParameters dtiparams = image.getDTIParameters();
		if ( dtiparams.getGradients() == null || dtiparams.getbValues() == null )
		{
			return estimate( image, maskImage, getMatrix(dtiparams), usePartialEstimates );
		}
		
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		int dimDW = image.getExtents().length > 3 ? image.getExtents()[3] : 1;
		
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
		double[] bvalues = new double[tempDimDW];
		double[][] grads = new double[tempDimDW][3];
		byte[][][] mask = null;
		if ( maskImage != null )
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
							if ( maskImage.getBoolean( z * dimY * dimX + y * dimX + x ) )
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
		ModelImage tensorImage = makeTensorImage( tensors, null, name );
		JDialogBase.updateFileInfo( image, tensorImage );
		return tensorImage;
	}
	
	
	public static void estimateBallAndStickCamino(float[][][][] DWdata,
			byte[][][] mask, BallStickInversion dtiFit, float[][][][] ballDiff,
			float[][][][] ballFrac, float[][][][] stickVec,
			float[][][][] exitcode, float[][][][] intensity) {
		//***************************************************
		// Step 1: Validate Input Arguments 
		//***************************************************		
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


		//***************************************************
		// Step 2: Loop over all voxels and estimate tensors 
		//***************************************************
		double data[] = new double[DWdata[0][0][0].length];
		for(int i=0;i<DWdata.length;i++) {
			for(int j=0;j<DWdata[0].length;j++) {
				for(int k=0;k<DWdata[0][0].length;k++) {
					if(mask[i][j][k]!=0) {
						for(int l=0;l<data.length;l++) {
							data[l]=DWdata[i][j][k][l];
						}				

						//						  * @return {errorCode, ln(S_0), d, f, x, y, z}. 

						double []estResult = dtiFit.invert(data);
						exitcode[i][j][k][0]=(float)estResult[0];
						intensity[i][j][k][0]=(float)Math.exp(estResult[1]);
						ballDiff[i][j][k][0]=(float)estResult[2];
						ballFrac[i][j][k][0]=(float)estResult[3];
						for(int l=0;l<3;l++) {
							stickVec[i][j][k][l]=(float)(estResult[4+2]*1e6);	
						}

					} else {
						exitcode[i][j][k][0]=Float.NaN;
						intensity[i][j][k][0]=Float.NaN;
						ballDiff[i][j][k][0]=Float.NaN;
						ballFrac[i][j][k][0]=Float.NaN;
						for(int l=0;l<3;l++) {
							stickVec[i][j][k][l]=Float.NaN;
						}
					}
				}
			}
		}

	}

	private static void estimateCamino(float [][][][]DWdata, 
			byte [][][]mask, int dtiType, DW_Scheme scheme, DT_Inversion dtiFit, 
			float [][][][]tensors,float [][][][]exitcode, float [][][][]intensity) {

		//***************************************************
		// Step 1: Validate Input Arguments 
		//**************************************************		
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


		//***************************************************
		// Step 2: Loop over all voxels and estimate tensors 
		//***************************************************
        final ViewJProgressBar progressBar = new ViewJProgressBar("tensor estimation",
                "estimating tensor...", 0, 100, false, null, null);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(0);
        
        boolean allZero = false;
		double data[] = new double[DWdata[0][0][0].length];
		for(int i=0;i<DWdata.length;i++) {
			for(int j=0;j<DWdata[0].length;j++) {
				for(int k=0;k<DWdata[0][0].length;k++) {
					if(mask[i][j][k]!=0) {
						allZero = true;
						for(int l=0;l<data.length;l++) {
							data[l]=DWdata[i][j][k][l];
							if ( data[l] != 0 )
							{
								allZero = false;
							}
						}				
						
						if ( !allZero  )
						{
							// if the type is something other than Nonlinear or Restore, compute the inversion.
							// if the type is either Nonlinear or Restore, check the mean...
							if ( !(dtiType == DTIPipeline.NON_LINEAR || dtiType == DTIPipeline.RESTORE) || (scheme.geoMeanZeroMeas(data) > 0) )
							{
								//						 {exitcode, ln A^\star(0), Dxx, Dxy, Dxz, Dyy, Dyz, Dzz}
								try {
									double []estResult = dtiFit.invert(data);
									exitcode[i][j][k][0]=(float)estResult[0];

									intensity[i][j][k][0]=(float)Math.exp(estResult[1]);

									for(int l=0;l<6;l++) {
										tensors[i][j][k][l]=(float)(estResult[l+2]*1e6);	
									}
								} catch ( misc.LoggedException e ) {}
							}
						}
						else
						{							
							for(int l=0;l<6;l++) {
								tensors[i][j][k][l]=0;	
							}
						}
					} else {
						exitcode[i][j][k][0]=Float.NaN;
						intensity[i][j][k][0]=Float.NaN;
						for(int l=0;l<6;l++) {
							tensors[i][j][k][l]=Float.NaN;	
						}
					}
				}
			}
			progressBar.updateValueImmed( (int)(100 * i / (float)DWdata.length) );
		}
        progressBar.updateValueImmed(100);
        progressBar.dispose();
	}
	/**
	 * Uses Camino algorithms to estimate tensors from DW imaging data on a voxel-wise basis. 
	 * @param image input DWI images series 4D
	 * @param maskImage input mask image (or null) 3D
	 * @param dtiType the type of estimation (Linear, Non-Linear, Restore, Weighted-Linear).
	 */
	public static ModelImage estimateCamino( ModelImage image, ModelImage maskImage, int dtiType,
			boolean bSaveExitCode, boolean bDisplayExitCode, boolean bSaveIntensity, boolean bDisplayIntensity,
			String kOutpuDir )
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
		int nB0 = 1 + (dimDW - tempDimDW);
		double[][] b0_data = null;
		if ( (dtiType == DTIPipeline.RESTORE) && (nB0 > 1) )
		{
			b0_data = new double[dimX*dimY*dimZ][nB0];
			int index = 0;
			for ( int dw = 0; dw < dimDW; dw++ )
			{
				if ( (dtiparams.getGradients()[dw][0] == 0) &&
						(dtiparams.getGradients()[dw][1] == 0) &&
						(dtiparams.getGradients()[dw][2] == 0) )
				{
					for ( int z = 0; z < dimZ; z++ )
					{
						for ( int y = 0; y < dimY; y++ )
						{
							for ( int x = 0; x < dimX; x++ )
							{
								b0_data[z*dimX*dimY + y*dimX + x][index] = image.getFloat( x, y, z, dw );
							}
						}
					}
					index++;
				}
			}
		}
		
		float[][][][] DWdata = new float[dimX][dimY][dimZ][tempDimDW];
		double[] bvalues = new double[tempDimDW];
		double[][] grads = new double[tempDimDW][3];
		byte[][][] mask = null;
		if ( maskImage != null )
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
							if ( maskImage.getBoolean( z * dimY * dimX + y * dimX + x ) )
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
		
		B_VectorScheme scheme = new B_VectorScheme(grads, bvalues);
		DT_Inversion dtiFit = null;
		switch ( dtiType )
		{
		case DTIPipeline.LINEAR:
			// can work w/BMatrix or b-vals/grads:
			dtiFit = new LinearDT_Inversion(scheme);
			break;
		case DTIPipeline.NON_LINEAR:
			// needs b-vals/grads:
			dtiFit = new NonLinearDT_Inversion(scheme);
			break;
		case DTIPipeline.RESTORE:
			// needs b-vals/grads:
			double[] noise_sd = new double[]{0,0};
			if ( nB0 == 2 )
			{
				noise_sd = EstimateSNR.snrDiff( b0_data );
			}
			else if ( nB0 > 2 )
			{
				noise_sd = EstimateSNR.snrMult( b0_data );
			}
	        System.err.println( (noise_sd[0]/noise_sd[1]) + "     " + noise_sd[0] + " " + noise_sd[1]);
			dtiFit = new RestoreDT_Inversion(scheme, noise_sd[1]);
			b0_data = null;
			break;
		case DTIPipeline.WEIGHTED_LINEAR:
			// can work w/BMatrix or b-vals/grads:
			dtiFit = new WeightedLinearDT_Inversion(scheme);
			break;
		}

		float [][][][] tensors = new float[dimX][dimY][dimZ][6];
		float [][][][] exitCode = new float[dimX][dimY][dimZ][1];
		float [][][][] intensity = new float[dimX][dimY][dimZ][1];

		estimateCamino( DWdata, mask, dtiType, scheme, dtiFit, tensors, exitCode, intensity );
		String name = JDialogBase.makeImageName(image.getImageName(), "_tensor");	
		ModelImage tensorImage = makeTensorImage( tensors, exitCode, name );
		JDialogBase.updateFileInfo( image, tensorImage );
		
		if ( bSaveExitCode )
		{
			name = JDialogBase.makeImageName(image.getImageName(), "_ExitCode");	
			ModelImage exitCodeImage = makeImage( exitCode, name );
			JDialogBase.updateFileInfo( image, exitCodeImage );
			ModelImage.saveImage( exitCodeImage, exitCodeImage.getImageName() + ".xml", kOutpuDir );
			if ( bDisplayExitCode )
			{
				new ViewJFrameImage( exitCodeImage );
			}
			else
			{
				exitCodeImage.disposeLocal();
				exitCodeImage = null;
			}
		}
		
		if ( bSaveIntensity )
		{
			name = JDialogBase.makeImageName(image.getImageName(), "_Intensity");	
			ModelImage intensityImage = makeImage( intensity, name );
			JDialogBase.updateFileInfo( image, intensityImage );
			ModelImage.saveImage( intensityImage, intensityImage.getImageName() + ".xml", kOutpuDir );
			if ( bDisplayIntensity )
			{
				new ViewJFrameImage( intensityImage );
			}
			else
			{
				intensityImage.disposeLocal();
				intensityImage = null;
			}
		}
		
		
		return tensorImage;
	}

	public static void estimateCaminoRESTORE(float [][][][]DWdata, 
			byte [][][]mask, float[][][] noiseField, 
			float [][][][]tensors,float [][][][]exitcode, float [][][][]intensity, DW_Scheme DTIscheme) {

		//***************************************************
		// Step 1: Validate Input Arguments 
		//***************************************************		
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


		//***************************************************
		// Step 2: Loop over all voxels and estimate tensors 
		//***************************************************
		double data[] = new double[DWdata[0][0][0].length];
		for(int i=0;i<DWdata.length;i++) {
			System.out.println("jist.plugins"+"\t"+"Row: "+i+" of "+DWdata.length);
			for(int j=0;j<DWdata[0].length;j++) {
				for(int k=0;k<DWdata[0][0].length;k++) {
					if(mask[i][j][k]!=0) {
						for(int l=0;l<data.length;l++) {
							data[l]=DWdata[i][j][k][l];
						}				

						//							 {exitcode, ln A^\star(0), Dxx, Dxy, Dxz, Dyy, Dyz, Dzz}
						float nf = noiseField[i][j][k];
						if(Float.isInfinite(nf)||Float.isNaN(nf)) {
							// Skip it.
							exitcode[i][j][k][0]=Float.NaN;
							intensity[i][j][k][0]=Float.NaN;
							for(int l=0;l<6;l++) {
								tensors[i][j][k][l]=Float.NaN;	
							}
						} else {
							DT_Inversion dtiFit=new RestoreDT_Inversion(DTIscheme,nf);
							double []estResult = dtiFit.invert(data);
							exitcode[i][j][k][0]=(float)estResult[0];
							intensity[i][j][k][0]=(float)Math.exp(estResult[1]);
							for(int l=0;l<6;l++) {
								tensors[i][j][k][l]=(float)(estResult[l+2]*1e6);	
							}
						}

					} else {
						exitcode[i][j][k][0]=Float.NaN;
						intensity[i][j][k][0]=Float.NaN;
						for(int l=0;l<6;l++) {
							tensors[i][j][k][l]=Float.NaN;	
						}
					}
				}
			}
		}
	}

	public static void estimateCaminoThreeTensor(float[][][][] DWdata,
			byte[][][] mask, ThreeTensorInversion dtiFit, float[][][][] mix1,
			float[][][][] tensors1, float[][][][] mix2, float[][][][] tensors2,
			float[][][][] mix3, float[][][][] tensors3, float[][][][] exitcode,
			float[][][][] intensity) {
		//***************************************************
		// Step 1: Validate Input Arguments 
		//***************************************************		
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


		//***************************************************
		// Step 2: Loop over all voxels and estimate tensors 
		//***************************************************
		double data[] = new double[DWdata[0][0][0].length];
		for(int i=0;i<DWdata.length;i++) {
			for(int j=0;j<DWdata[0].length;j++) {
				for(int k=0;k<DWdata[0][0].length;k++) {
					if(mask[i][j][k]!=0) {
						for(int l=0;l<data.length;l++) {
							data[l]=DWdata[i][j][k][l];
						}				

						//						 * @return {exitcode, ln A^\star(0), 2, mix1, D1xx, D1xy, D1xz, D1yy, D1yz,
						//					     *         D1zz, mix2, D2xx, D2xy, D2xz, D2yy, D2yz, D2zz}
						double []estResult = dtiFit.invert(data);
						exitcode[i][j][k][0]=(float)estResult[0];
						intensity[i][j][k][0]=(float)Math.exp(estResult[1]);
						mix1[i][j][k][0]=(float)estResult[3];
						mix2[i][j][k][0]=(float)estResult[10];
						mix3[i][j][k][0]=(float)estResult[17];
						for(int l=0;l<6;l++) {
							tensors1[i][j][k][l]=(float)(estResult[l+4]*1e6);
							tensors2[i][j][k][l]=(float)(estResult[l+11]*1e6);
							tensors3[i][j][k][l]=(float)(estResult[l+18]*1e6);
						}

					} else {
						exitcode[i][j][k][0]=Float.NaN;
						intensity[i][j][k][0]=Float.NaN;
						mix1[i][j][k][0]=Float.NaN;
						mix2[i][j][k][0]=Float.NaN;
						mix3[i][j][k][0]=Float.NaN;
						for(int l=0;l<6;l++) {
							tensors1[i][j][k][l]=Float.NaN;
							tensors2[i][j][k][l]=Float.NaN;
							tensors3[i][j][k][l]=Float.NaN;
						}
					}
				}
			}
		}
	}

	public static void estimateCaminoTwoTensor(float[][][][] DWdata,
			byte[][][] mask, TwoTensorInversion dtiFit, float[][][][] mix1,
			float[][][][] tensors1, float[][][][] mix2, float[][][][] tensors2,
			float[][][][] exitcode, float[][][][] intensity) {

		//***************************************************
		// Step 1: Validate Input Arguments 
		//***************************************************		
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


		//***************************************************
		// Step 2: Loop over all voxels and estimate tensors 
		//***************************************************
		double data[] = new double[DWdata[0][0][0].length];
		for(int i=0;i<DWdata.length;i++) {
			for(int j=0;j<DWdata[0].length;j++) {
				for(int k=0;k<DWdata[0][0].length;k++) {
					if(mask[i][j][k]!=0) {
						for(int l=0;l<data.length;l++) {
							data[l]=DWdata[i][j][k][l];
						}				

						//						 * @return {exitcode, ln A^\star(0), 2, mix1, D1xx, D1xy, D1xz, D1yy, D1yz,
						//					     *         D1zz, mix2, D2xx, D2xy, D2xz, D2yy, D2yz, D2zz}
						double []estResult = dtiFit.invert(data);
						exitcode[i][j][k][0]=(float)estResult[0];
						intensity[i][j][k][0]=(float)Math.exp(estResult[1]);
						mix1[i][j][k][0]=(float)estResult[3];
						mix2[i][j][k][0]=(float)estResult[10];
						for(int l=0;l<6;l++) {
							tensors1[i][j][k][l]=(float)(estResult[l+4]*1e6);
							tensors2[i][j][k][l]=(float)(estResult[l+11]*1e6);
						}

					} else {
						exitcode[i][j][k][0]=Float.NaN;
						intensity[i][j][k][0]=Float.NaN;
						mix1[i][j][k][0]=Float.NaN;
						mix2[i][j][k][0]=Float.NaN;
						for(int l=0;l<6;l++) {
							tensors1[i][j][k][l]=Float.NaN;
							tensors2[i][j][k][l]=Float.NaN;
						}
					}
				}
			}
		}
	}


	public static void main(String args[]) {
		/****************************************************
		 * Run the testing script by default. 
		 ****************************************************/
		test();
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
		double []bvalues = new double[17];
		double [][]grads = new double[17][3];

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
					bvalues[j]=1000.0*r.nextDouble()+100;
					double norm =0;
					for(int k=0;k<3;k++) {
						grads[j][k] = r.nextDouble()-0.5;
						norm+=grads[j][k]*grads[j][k];
					}
					norm=Math.sqrt(norm);
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
			//			double []bvalues, double [][]grads, boolean [][][]mask
		}
		if(failed==0) {
			System.out.println("jist.plugins"+"\t"+"Random Testing Passed.");
			return true;
		} else {
			System.out.println("jist.plugins"+"\t"+"**********Random Testing Failed.*************");
			return false;
		}
	}

	/**
	 * Creates and returns a ModelImage with the tensor data.
	 * @param tensors tensor data
	 * @param name name of the ModelImage
	 * @return new ModelImage.
	 */
	private static ModelImage makeTensorImage( float[][][][] tensors, float[][][][] exitCode, String name  )
	{
		int dimX = tensors.length;
		int dimY = tensors[0].length;
		int dimZ = tensors[0][0].length;
		ModelImage tensorImage = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ,6}, name );
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( (exitCode == null) || ((exitCode != null) && (exitCode[x][y][z][0] == 0)) )
					{
						for (int i = 0; i < 6; i++) {
							tensors[x][y][z][i] = ( tensors[x][y][z][i] * 1000000); // um^2/sec
							if (i == 0 && tensors[x][y][z][0] < 0) {
								tensors[x][y][z][0] = (float) 0.01;
							}
							if (i == 3 && tensors[x][y][z][3] < 0) {
								tensors[x][y][z][3] = (float) 0.01;
							}
							if (i == 5 && tensors[x][y][z][5] < 0) {
								tensors[x][y][z][5] = (float) 0.01;
							}
							tensorImage.set( x, y, z, i, tensors[x][y][z][i] );
						}
					}
					else
					{
						tensorImage.set( x, y, z, 0, 0 );
						tensorImage.set( x, y, z, 1, 0 );
						tensorImage.set( x, y, z, 2, 0 );
						tensorImage.set( x, y, z, 3, 0 );
						tensorImage.set( x, y, z, 4, 0 );
						tensorImage.set( x, y, z, 5, 0 );
					}
				}
			}
		}
		tensorImage.calcMinMax();
		return tensorImage;
	}
	
	/**
	 * Creates and returns a ModelImage with the tensor data.
	 * @param tensors tensor data
	 * @param name name of the ModelImage
	 * @return new ModelImage.
	 */
	private static ModelImage makeImage( float[][][][] values, String name  )
	{
		int dimX = values.length;
		int dimY = values[0].length;
		int dimZ = values[0][0].length;
		ModelImage image = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ}, name );
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					image.set( x, y, z, values[x][y][z][0] );
				}
			}
		}
		image.calcMinMax();
		return image;
	}
	
	private static GMatrixd getMatrix( DTIParameters dtiparams )
	{
		if ( dtiparams.getbMatrixVals() == null )
		{
			return null;
		}

		double[][] bMatrix = dtiparams.getbMatrixVals();
		GMatrixd gMatrix = new GMatrixd( bMatrix.length, 7 );
		for ( int i = 0; i < bMatrix.length; i++ )
		{
			for ( int j = 0; j < 6; j++ )
			{
				gMatrix.Set(i, j, bMatrix[i][j]);
			}
			gMatrix.Set(i, 6, 1);	
		}
		return gMatrix;
	}
}
