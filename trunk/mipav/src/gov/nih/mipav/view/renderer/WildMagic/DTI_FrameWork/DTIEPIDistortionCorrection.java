package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import Jama.Matrix;

public class DTIEPIDistortionCorrection {
	
	//Internal Variables
	int XN, YN, ZN; 
	float[] dimRes;
	int chN = 3;
	
	public void runAlgorithm() {
		/*this.setLabel("combine Volumes");
		
		ImageData EPICorrectDef = inParamEPICorrection.getImageData(); 
		
		ImageData[] currentDef = new ImageDataFloat[chN];
		ImageData[] EPICorrectDefSplit = RegistrationUtilities.split4DImageDataIntoArray(EPICorrectDef);
		XN = EPICorrectDef.getRows();
		YN = EPICorrectDef.getCols();
		ZN = EPICorrectDef.getSlices();

		dimRes = EPICorrectDef.getHeader().getDimResolutions();
		//Matrix inMatrix =  inParamMatrix.getValue(); 
		
		//currentDef = applyTransformation(currentDef, inMatrix);
		
		//List<File> DWtoB0Trans = inParamDWtoB0Transform.getValue();
		//List<File> correctDWtoStructTrans = inParamTransDWCorrectedToStruct.getValue();
		ArrayList<double[][]> arrayDWtoB0Trans = readMultiMatrices(inParamDWtoB0Transform.getValue());
		//ArrayList<double[][]> arraycorrectDWtoStructTrans = readMultiMatrices(inParamRigidB0toStruct.getValue());
		Matrix m;
		Matrix rigidB0toStruct = inParamRigidB0toStruct.getValue();
		double[][] matarray;
		//if (arrayDWtoB0Trans.size() != arraycorrectDWtoStructTrans.size()) System.out.format("Transform Size Mismatch!");
		
		for (int i = 0; i < arrayDWtoB0Trans.size(); i++){
			//File currentFile = DWtoB0Trans.get(i);
			//apply transformations or deformations one by one
			//currentDef = new ImageDataFloat[chN];
			 
			
				m = new Matrix(4,4);
				matarray = arrayDWtoB0Trans.get(i);
				if(matarray.length!=4 || matarray[0].length!=4){
					System.err.println(getClass().getCanonicalName()+"Invalid transformation - must be 4x4");
				}else{
					for(int k=0; k<matarray.length; k++){
						for(int j=0; j<matarray[0].length; j++){
							m.set(k, j, matarray[k][j]);
						}
					}
				}
			
				m = rigidB0toStruct.times(m);
				System.out.format("hi\n");
				for(int k=0;k<4;k++)for(int l=0;l<4;l++)System.out.format(m.get(k,l)+"\n");
				
			currentDef=createDefFieldFromTransMatrix(m);
			currentDef=applyDeformation(currentDef, EPICorrectDefSplit);
			for(int c = 0; c < chN; c++) {
				currentDef[c].setName(inParamDWtoB0Transform.getValue().get(i).getName().replace(".","_") + "_combDefField");
				currentDef[c].setHeader(inParamEPICorrection.getImageData().getHeader());
			}
			
			ImageData outVolume = RegistrationUtilities.combineImageDataArrayTo4D(currentDef);
				//outVolume.setHeader(inParamEPICorrection.getImageData().getHeader());
				//outVolume.setName(inParamDWtoB0Transform.getValue().get(i).getName().replace(".","_") + "_combDefField");
				outParamCombDefField.add(outVolume);
				outParamCombDefField.writeAndFreeNow(alg);

		}*/
		
	}
	
	private ArrayList<double[][]> readMultiMatrices(List<File> files){
		ArrayList<double[][]> allxfms = new ArrayList<double[][]>(files.size());
		int i=0;
		while(i<files.size()){
			String fileName = files.get(i).getName();
			if (fileName == null) {
				return null;
			}
			if (!files.get(i).exists()) {
				return null;
			}
			try {
			allxfms.add(readObject(files.get(i)));
			}
			catch(Exception e) {
				e.printStackTrace();
				return null;
			}
			i++;
		}
		return allxfms;
	}
	
	protected double[][] readObject(File f) {
		BufferedReader in;
		// ArrayList<ArrayList<Double>> datArray=new
		// ArrayList<ArrayList<Double>>();
		// ArrayList<Double> array;
		try {
			// Create input stream from file
			in = new BufferedReader(new InputStreamReader(
					new FileInputStream(f)));
			String str;
			// Read file as string
			int rows = Integer.parseInt(in.readLine());
			int cols = Integer.parseInt(in.readLine());
			double[][] dat = new double[rows][cols];
			int i = 0;
			while ((str = in.readLine()) != null && i<rows) {
				// array=new ArrayList<Double>();
				String[] strs = str.split(" ");
				int j = 0;
				for (String s : strs) {
					try {
						// array.add(Double.parseDouble(s));
						dat[i][j] = Double.parseDouble(s);
					} catch (NumberFormatException e) {
					}
					j++;
				}
				// datArray.add(array);
				i++;
			}
			in.close();
			// double[][] dat=new double[datArray.size()][0];
			// for(int i=0;i<datArray.size();i++){
			// array=datArray.get(i);
			// dat[i]=new double[array.size()];
			// for(int j=0;j<array.size();j++){
			// dat[i][j]=array.get(j);
			// }
			// }
			return dat;
		} catch (Exception e) {
			System.err.println(getClass().getCanonicalName()+"Error occured while reading parameter file:\n"
					+ e.getMessage());
			e.printStackTrace();
			return null;
		}
	}
	
	public float[][][][] applyDeformation(float[][][][] currentDef, double[][][][] epiDef){
		double[] currentVec = new double[chN];
		double[] newVec = new double[chN];
		
		float[][][][] newDef = new float[chN][XN][YN][ZN];
		for (int c = 0; c < chN; c++) {
			for(int i = 0; i < XN; i++) {
				for(int j = 0; j < YN; j++) {
					for(int k = 0; k < ZN; k++){
					   newDef[c][i][j][k] = currentDef[c][i][j][k];	
					}
				}
			}
		}
		

		for(int i = 0; i < XN; i++)
			for(int j = 0; j < YN; j++)
				for(int k = 0; k < ZN; k++){
					
					for(int c = 0; c < chN; c++) newVec[c] = epiDef[c][i][j][k]; 
					
					//get current deformation at where the new deformation is pointing
					for(int c = 0; c < chN; c++) currentVec[c] = TrilinearInterpolation(currentDef[c], XN, YN, ZN, 
							newVec[0]+i, newVec[1]+j, newVec[2]+k); 
					
					for(int c = 0; c < chN; c++) newDef[c][i][j][k] = (float)(newVec[c]+currentVec[c]);
					
				}
		return newDef;			
		
	}
	
	public double TrilinearInterpolation(float[][][] oldV, int XN, int YN,
			int ZN, double x, double y, double z) {
		int i0, j0, k0, i1, j1, k1;
		double dx, dy, dz, hx, hy, hz;
		if (x < 0 || x > (XN - 1) || y < 0 || y > (YN - 1) || z < 0
				|| z > (ZN - 1)) {
			return 0;
		} else {
			j1 = (int) Math.ceil(x);
			i1 = (int) Math.ceil(y);
			k1 = (int) Math.ceil(z);
			j0 = (int) Math.floor(x);
			i0 = (int) Math.floor(y);
			k0 = (int) Math.floor(z);
			dx = x - j0;
			dy = y - i0;
			dz = z - k0;

			// Introduce more variables to reduce computation
			hx = 1.0 - dx;
			hy = 1.0 - dy;
			hz = 1.0 - dz;
			// Optimized below
			return   (((oldV[j0][i0][k0] * hx + oldV[j1][i0][k0] * dx) * hy 
					 + (oldV[j0][i1][k0] * hx + oldV[j1][i1][k0] * dx) * dy) * hz 
					+ ((oldV[j0][i0][k1] * hx + oldV[j1][i0][k1] * dx) * hy 
					 + (oldV[j0][i1][k1] * hx + oldV[j1][i1][k1] * dx) * dy)* dz);

		}
	}
	
	public float[][][][] createDefFieldFromTransMatrix(Matrix newTrans){
		// newTrans is 4 x 4
		// double[] currentVec = new double[chN];
		double[] newVec = new double[chN];
		float[][][][] newDef = new float[chN][XN][YN][ZN];
		
		for(int i = 0; i < XN; i++)
			for(int j = 0; j < YN; j++)
				for(int k = 0; k < ZN; k++){
					
					newVec[0] = i*dimRes[0];
					newVec[1] = j*dimRes[1];
					newVec[2] = k*dimRes[2];
					                	
                    Matrix v = new Matrix(new double[]{newVec[0],newVec[1],newVec[2],1},4);
                    /** Construct a matrix from a one-dimensional packed array
                    @param vals One-dimensional array of doubles, packed by columns (ala Fortran).
                    @param m    Number of rows.
                    @exception  IllegalArgumentException Array length must be a multiple of m.
                    */

                    /*public Matrix (double vals[], int m) {
                       this.m = m;
                       n = (m != 0 ? vals.length/m : 0);
                       if (m*n != vals.length) {
                          throw new IllegalArgumentException("Array length must be a multiple of m.");
                       }
                       A = new double[m][n];
                       for (int i = 0; i < m; i++) {
                          for (int j = 0; j < n; j++) {
                             A[i][j] = vals[i+j*m];
                          }
                       }
                    }*/
                    /*
                     * m = 4;
                     * n = 1;
                     * v = new double[4][1];
                     * v[0][0] = newVec[0];
                     * v[1][0] = newVec[1];
                     * v[2][0] = newVec[2];
                     * v[3][0] = 1;
                     */
                    Matrix vp = newTrans.solve(v);
                    /** Solve A*X = B
                    @param B    right hand side
                    @return     solution if A is square, least squares solution otherwise
                    */
                    for(int c = 0; c < chN; c++) {
                        newVec[c] = vp.get(c,0) - newVec[c]; 
                    }
                    
					//get current deformation at where the new deformation is pointing
					//for(int c = 0; c < chN; c++) currentVec[c] = RegistrationUtilities.Interpolation(currentDef[c], XN, YN, ZN, 
					//		newVec[0]+i, newVec[1]+j, newVec[2]+k, InterpolationType.TRILINEAR); 

					//for(int c = 0; c < chN; c++) newDef[c].set(i,j,k, newVec[c]+currentVec[c]);
					
					for(int c = 0; c < chN; c++) newDef[c][i][j][k] = (float)( newVec[c]/dimRes[c]);
					
				}
		
		return newDef;
		
	}
}