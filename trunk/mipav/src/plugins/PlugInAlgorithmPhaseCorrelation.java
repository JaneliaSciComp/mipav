import java.awt.Point;
import java.io.IOException;
import java.util.ArrayList;
import java.util.PriorityQueue;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmFFT2;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


public class PlugInAlgorithmPhaseCorrelation extends AlgorithmBase {
	
	private ModelImage imageA;
	
	private ModelImage imageB;
	
	//private ModelImage dstImage;
	
	private ArrayList<Vector3f> peaks;
	
	private Vector3f maxPt;
	
	private int width;
	
	private int height;
	
	private int depth;
	
	private int sliceSize;
	
	private int length;
	
	private int[] extents;
	
	private int[] bufferA;
	
	private int[] bufferB;
	
	private float[] corrBuffer;
	
	private int numMax;
	
	private boolean greenLight = true;
	
	private int overlap;
	
	private int[] original;
	
	private ModelImage origA;
	
	private ModelImage origB;
	
	private Point baseTranslate = null;
	
	private int window = -1;
	
	private boolean requiresSetup;
	
	private Vector3f subpixelShift;
	
	private Vector3f subpixelShiftA;
	
	private Vector3f subpixelShiftB;
	
	public PlugInAlgorithmPhaseCorrelation(ModelImage _imageA, ModelImage _imageB,
			ModelImage FFTimA, ModelImage FFTimB, int maxPeaks, int overlapPct){
		
		origA = _imageA;
		origB = _imageB;
		
		original = origA.getExtents();
		
		int[] extentsB = origB.getExtents();
		
		/*if(original[0]%2 != 0 || original[1]%2 != 0){
			MipavUtil.displayError("Image dimensions must be even");
			greenLight = false;
			return;
		}*/
		if(original.length != extentsB.length || original[0] != extentsB[0] 
				|| original[1] != extentsB[1]){
			MipavUtil.displayError("Image dimensions do not match");
			greenLight = false;
			return;
		}
		else if(extentsB.length>2 && original[2] != extentsB[2]){
			MipavUtil.displayError("Image dimensions do not match");
			greenLight = false;
			return;
		}
		
		imageA = FFTimA;
		imageB = FFTimB;
		numMax = maxPeaks;
		overlap = overlapPct;
		
		extents = imageA.getExtents();
		
		width = extents[0];
		height = extents[1];
		depth = 1;
		
		if(extents.length>2)
			depth = extents[2];
		
		
		sliceSize = width*height;
		length = sliceSize*depth;
		
		bufferA = new int[original[0]*original[1]];
		bufferB = new int[original[0]*original[1]];
		
		peaks = new ArrayList<Vector3f>();
		
		requiresSetup = false;
		
	}
	
	public PlugInAlgorithmPhaseCorrelation(ModelImage _imageA, ModelImage _imageB, Vector3f translate,
			int maxPeaks, int windowSize, int overlapPct){
		
		origA = _imageA;
		origB = _imageB;
		baseTranslate = new Point((int)translate.X, (int)translate.Y);
		window = windowSize;
		
		original = origA.getExtents();
		
		int[] extentsB = origB.getExtents();
		
		if(original[0]%2 != 0 || original[1]%2 != 0){
			MipavUtil.displayError("Image dimensions must be even");
			greenLight = false;
			return;
		}
		if(original.length != extentsB.length || original[0] != extentsB[0] 
				|| original[1] != extentsB[1]){
			MipavUtil.displayError("Image dimensions do not match");
			greenLight = false;
			return;
		}
		else if(extentsB.length>2 && original[2] != extentsB[2]){
			MipavUtil.displayError("Image dimensions do not match");
			greenLight = false;
			return;
		}
		
		numMax = maxPeaks;
		overlap = overlapPct;
		
		bufferA = new int[original[0]*original[1]];
		bufferB = new int[original[0]*original[1]];
		
		peaks = new ArrayList<Vector3f>();
		
		requiresSetup = true;
		
	}
	
	public Vector3f getSubpixelShift(){
		return subpixelShift;
	}
	
	public void getSubpixelShiftTest(Vector3f a, Vector3f b){
		a.copy(subpixelShiftA);
		b.copy(subpixelShiftB);
	}
	
	public Vector3f getTranslation(){
		return maxPt;
	}
	
	/*public ModelImage getPeakImage(){
		return dstImage;
	}*/
	
	public ArrayList<Vector3f> getPeakList(){
		return peaks;
	}
	
	public boolean goodToGo(){
		return greenLight;
	}

	public void setWindow(int size){
		window = size;
	}
	
	@Override
	public void runAlgorithm() {

		if(requiresSetup){
			imageA = new ModelImage(ModelImage.COMPLEX, original, "FFTA");
			imageB = new ModelImage(ModelImage.COMPLEX, original, "FFTB");
			AlgorithmFFT2 fftA = new AlgorithmFFT2(imageA, origA, AlgorithmFFT2.FORWARD, false, false, false,false);
			fftA.run();
			AlgorithmFFT2 fftB = new AlgorithmFFT2(imageB, origB, AlgorithmFFT2.FORWARD, false, false, false,false);
			fftB.run();
			
			extents = imageA.getExtents();
			width = extents[0];
			height = extents[1];
			depth = 1;
			
			if(extents.length>2)
				depth = extents[2];
			
			
			sliceSize = width*height;
			length = sliceSize*depth;
		}
		
		float[] real1 = new float[length];
		float[] imag1 = new float[length];
		float[] real2 = new float[length];
		float[] imag2 = new float[length];
		
		try {
			origA.exportData(0, original[0]*original[1], bufferA);
			origB.exportData(0, original[0]*original[1], bufferB);
			imageA.exportComplexData(0, length, real1, imag1);
			imageB.exportComplexData(0, length, real2, imag2);
		} catch (IOException e1) {
			e1.printStackTrace();
		}

		float[] realP = new float[length];
		float[] imagP = new float[length];
		float mag;
		
		for(int i=0;i<length;i++){
			realP[i] = real1[i]*real2[i] + imag1[i]*imag2[i];
			imagP[i] = real2[i]*imag1[i] - real1[i]*imag2[i];
			mag = (float)Math.sqrt(realP[i]*realP[i] + imagP[i]*imagP[i]);
			realP[i] /= mag;
			imagP[i] /= mag;
		}
		
		ModelImage complex = new ModelImage(ModelImage.COMPLEX, extents,  "Intermediate");
		try {
			complex.importComplexData(0, realP, imagP, true, false);
		} catch (IOException e) {
			e.printStackTrace();
		}
		AlgorithmFFT2 ifft = new AlgorithmFFT2(complex, AlgorithmFFT2.INVERSE, false, false, false, true);
		ifft.run();
		
		double[] realCorr = ifft.getRealData();
		double[] imagCorr = ifft.getImaginaryData();
		corrBuffer = new float[realCorr.length];
		
		for(int i=0;i<realCorr.length;i++){
			corrBuffer[i] = (float) Math.sqrt(realCorr[i]*realCorr[i] + imagCorr[i]*imagCorr[i]);
		}
		
		//corrBuffer = ifft.getRealData();
		complex.disposeLocal();
		//cropBack();
		
		/*ModelImage dstImage = new ModelImage(ModelImage.FLOAT, extents, "Result");
		try {
			dstImage.importData(0, corrBuffer, true);
		} catch (IOException e) {
			e.printStackTrace();
		}*/
		
		/*ViewJFrameImage frame = new ViewJFrameImage(complex);
		frame.setVisible(true);*/
		
		PriorityQueue<MaxCoord> pq = new PriorityQueue<MaxCoord>();
		MaxCoord head;
		
		int x, y, z;
		float val;
		/*for(int i=0;i<length;i++){
			x = i%width;
			y = (i/width)%height;
			z = i/sliceSize;
			val = corrBuffer[i];
			if(isLocalMax(x,y, z)){
				if(pq.size() < numMax){
					pq.add(new MaxCoord(x,y,z, val));
				}
				else{
					head = pq.peek();
					if(corrBuffer[i] > head.corr){
						pq.remove();
						pq.add(new MaxCoord(x,y,z, val));
					}
				}
			}
		}*/
		
		for(int j=0;j<height;j++){
			for(int i=0;i<width;i++){
				val = corrBuffer[i+j*width];
				if(isLocalMax(i,j)){
					if(pq.size() < numMax){
						pq.add(new MaxCoord(i,j,0, val));
						//System.out.println(i + " " + j + " " + val);
					}
					else{
						head = pq.peek();
						if(val > head.corr){
							pq.poll();
							pq.add(new MaxCoord(i,j,0, val));
							//System.out.println(i + " " + j + " " + val);
						}
					}
				}

			}
		}	
		
		double maxR = Float.NEGATIVE_INFINITY;
		
		/*if(extents.length>2){
			maxPt = new Vector3f();
			int[] dirX = {0, 0, 0, 0, 1, 1, 1, 1};
			int[] dirY = {0, 0, 1, 1, 0, 0, 1, 1};
			int[] dirZ = {0, 1, 0 ,1, 0, 1, 0, 1};
			double R;
			int nx, ny, nz;
	
			for(int i=0;i<numMax;i++){
				head = pq.poll();
				x = head.x;
				y = head.y;
				z = head.z;
				
				System.out.println(x + " " + y);
				for(int j=0;j<8;j++){
					nx = x - dirX[j]*width;
					ny = y - dirY[j]*height;
					nz = z - dirZ[j]*depth;
					R = corr(nx, ny, nz);
					//System.out.println((x - dirX[j]*width) + ", " + (y - dirY[j]*height));
					//System.out.println(R);
					if(R>maxR){
						maxR = R;
						maxPt.X = nx;
						maxPt.Y = ny;
						maxPt.Z = nz;
					}
				}
			}
			
			if(maxR == Float.NEGATIVE_INFINITY){
				maxPt = null;
			}
		}
		else{*/
			maxPt = new Vector3f();
			int[] dirX = {0, 0, 1, 1};
			int[] dirY = {0, 1, 0, 1};
			double R;
			int nx, ny;
			int qsize = pq.size();
	
			for(int i=0;i<qsize;i++){
				head = pq.poll();
				x = head.x;
				y = head.y;
				
				//System.out.println(x + " " + y + " " + head.corr);
				for(int j=0;j<4;j++){
					nx = x - dirX[j]*width;
					ny = y - dirY[j]*height;
					//nx = x - dirX[j]*original[0];
					//ny = y - dirY[j]*original[1];

					if(Math.abs(nx) >= original[0] ||
							Math.abs(ny) >= original[1])
						continue;
					
					if(baseTranslate != null){
						int wx0 = baseTranslate.x - window;
						int wx1 = baseTranslate.x + window;
						int wy0 = baseTranslate.y - window;
						int wy1 = baseTranslate.y + window;
						
						if(nx < wx0 || nx > wx1 || ny < wy0 || ny > wy1)
							continue;
						
					}
					
					
					
					R = corr(nx, ny);
					if(R>maxR){
						maxR = R;
						maxPt.X = nx;
						maxPt.Y = ny;
					}
					//System.out.println((x - dirX[j]*width) + ", " + (y - dirY[j]*height));
					//System.out.println(R);
				}
			}
			
			if(maxR == Float.NEGATIVE_INFINITY){
				maxPt = null;
				System.out.println("No max found");
			}else{
				
				System.out.println("This R is max: " + maxR);
				System.out.println("At: " + maxPt.X + ", " + maxPt.Y);
				
				int xp = (int)maxPt.X;
				int yp = (int)maxPt.Y;
				boolean circX = false;
				boolean circY = false;
				if(xp < 0){
					xp += width;
					circX = true;
				}
				if(yp < 0){
					yp += height;
					circX = true;
				}

				int xpp = xp + 1;
				int ypp = yp + 1;
				int xn = xp - 1;
				int yn = yp - 1;
				int jp = width * yp;
				

				if(xpp >= width)
					xpp = 0;
				if(ypp >= height)
					ypp = 0;
				if(xn < 0)
					xn = width - 1;
				if(yn < 0)
					yn = height - 1; 
				
				int jpp = width * ypp;
				int jn = width * yn;
				
				//float dX1 = ((corrBuffer[xpp + jp]*xpp + corrBuffer[xp + jp]*xp) / (corrBuffer[xpp+jp] + corrBuffer[xp + jp]))-1;
				//float dY1 = ((corrBuffer[xp+jpp]*ypp + corrBuffer[xp+jp]*yp)/(corrBuffer[xp+jpp] + corrBuffer[xp + jp]))-1;
				//float dX1 = ((corrBuffer[xpp + jp]*xpp + corrBuffer[xp + jp]*xp) / (corrBuffer[xpp+jp] + corrBuffer[xp + jp]));
				//float dY1 = ((corrBuffer[xp+jpp]*ypp + corrBuffer[xp+jp]*yp)/(corrBuffer[xp+jpp] + corrBuffer[xp + jp]));
				
				float c00r = (float)realCorr[xp + jp];
				float c00i = (float)imagCorr[xp + jp];
				
				float c10r = (float)realCorr[xpp + jp];
				float c10i = (float)imagCorr[xpp + jp];
				float c01r = (float)realCorr[xp + jpp];
				float c01i = (float)imagCorr[xp + jpp];
				
				float c10rn = (float)realCorr[xn + jp];
				float c10in = (float)imagCorr[xn + jp];
				float c01rn = (float)realCorr[xp + jn];
				float c01in = (float)imagCorr[xp + jn];
				
				float xpMag = c10r*c10r + c10i*c10i;
				float xnMag = c10rn*c10rn + c10in*c10in;
				float ypMag = c01r*c01r + c01i*c01i;
				float ynMag = c01rn*c01rn + c01in*c01in;
				
				//float sgnX = 1.0f;
				//float sgnY = 1.0f;
				if(xnMag > xpMag){
					c10r = c10rn;
					c10i = c10in;
					//sgnX = -1.0f;
				}
				if(ynMag > ypMag){
					c01r = c01rn;
					c01i = c01in;
					//sgnY = -1.0f;
				}
				
				float dXr1 = c10r + c00r;
				float dXi1 = c10i + c00i;
				float dXr2 = c10r - c00r;
				float dXi2 = c10i - c00i;
				
				float dYr1 = c01r + c00r;
				float dYi1 = c01i + c00i;
				float dYr2 = c01r - c00r;
				float dYi2 = c01i - c00i;
				
				float[] dX1a = complexDivision(c10r, c10i, dXr1, dXi1);
				float[] dX2a = complexDivision(c10r, c10i, dXr2, dXi2);
				float[] dY1a = complexDivision(c01r, c01i, dYr1, dYi1);
				float[] dY2a = complexDivision(c01r, c01i, dYr2, dYi2);
				
				float dX1 = dX1a[0];
				float dX2 = dX2a[0];
				float dY1 = dY1a[0];
				float dY2 = dY2a[0];
				/*float dX1 = (float)Math.sqrt(dX1a[0]*dX1a[0] + dX1a[1]*dX1a[1]); 
				float dX2 = (float)Math.sqrt(dX2a[0]*dX2a[0] + dX2a[1]*dX2a[1]); 
				float dY1 = (float)Math.sqrt(dY1a[0]*dY1a[0] + dY1a[1]*dY1a[1]); 
				float dY2 = (float)Math.sqrt(dY2a[0]*dY2a[0] + dY2a[1]*dY2a[1]); */
				
				//float dX1 = corrBuffer[xpp + jp] / (corrBuffer[xpp + jp] + corrBuffer[xp + jp]);
				//float dX2 = corrBuffer[xpp + jp] / (corrBuffer[xpp + jp] - corrBuffer[xp + jp]);
				//if(dX1<-1 || dX1 > 1)
				//	dX1 = corrBuffer[xpp + jp] / (corrBuffer[xpp + jp] - corrBuffer[xp + jp]);
				//float dY1 = corrBuffer[xp + jpp] / (corrBuffer[xp + jpp] + corrBuffer[xp + jp]);
				//float dY2 = corrBuffer[xp + jpp] / (corrBuffer[xp + jpp] - corrBuffer[xp + jp]);
				//if(dY1<-1 || dY1 > 1)
				//	dY1 = corrBuffer[xp + jpp] / (corrBuffer[xp + jpp] - corrBuffer[xp + jp]);
				
				float dX;
				float dY;
				
				if(dX1<-1 || dX1 > 1){
					//dX = xp - sgnX * dX2;
					dX = xp - dX2;
				}else{
					//dX = xp - sgnX * dX1;
					dX = xp - dX1;
				}
				
				if(dY1<-1 || dY1 > 1){
					//dY = yp - sgnY * dY2;
					dY = yp - dY2;
				}else{
					//dY = yp - sgnY * dY1;
					dY = yp - dY1;
				}
				
				subpixelShiftA = new Vector3f(dX1, dY1, 0);
				subpixelShiftB = new Vector3f(dX2, dY2, 0);
				
				//dX1 += xp;//Might actually be xp - dx1?
				//dY1 += yp;
				
				//float dX, dY;
				
				/*if(dX1 < height/2){
					//dY = dX1;
					dX = dX1;
				}else{
					dX = dX1 - width;
					//dY = dX1 - width;
				}
				
				if(dY1 < width / 2){
					//dX = dY1;
					dY = dY1;
				}else{
					dY = dY1 - height;
					//dX = dY1 - height;
				}*/
				
				//maxPt.X += dX;
				//maxPt.Y += dY;
				if(circX)
					dX -= width;
				if(circY)
					dY -= width;
				
				subpixelShift = new Vector3f(dX, dY, 0);
				
				
				
				
			}
			
		//}
		
	}
	
	private float[] complexDivision(float a, float b, float c, float d){
		
		float denom = c*c - d*d;
		float realNum = (a*c + b*d) / denom;
		float imagNum = (b*c - a*d) / denom;
		
		return new float[]{realNum, imagNum};
		
	}
	
	private double corr(int ptX, int ptY){
		double R = 0;
		int width, height;
		int origWidth = original[0];
		int origHeight = original[1];
		int offsetXa, offsetYa;
		int offsetXb, offsetYb;
		
		if(ptX >= 0){
			offsetXa = ptX;
			offsetXb = 0;
			width = origWidth - ptX;
			//width = this.width - ptX;
		}
		else{
			offsetXa = 0;
			offsetXb = - ptX;
			width = origWidth + ptX;
			//width = this.width + ptX;
		}
		if(ptY >= 0){
			offsetYa = ptY;
			offsetYb = 0;
			height = origHeight - ptY;
			//height = this.height - ptY;
		}
		else{
			offsetYa = 0;
			offsetYb = - ptY;
			height = origHeight + ptY;
			//height = this.height + ptY;
		}
		
		/*if (100*width*height / sliceSize > 20)
			return Float.NEGATIVE_INFINITY;*/
		
		int ind, mind;
		int ya, yb;
		double aMean = 0;
		double bMean = 0;
		int count = 0;
		
		for(int j=0;j<height;j++){
			ya = (j+offsetYa) * origWidth;
			yb = (j+offsetYb) * origWidth;
			for(int i=0;i<width;i++){
				ind = (i + offsetXa) + ya ;
				mind = (i + offsetXb) + yb;
				aMean += bufferA[ind];
				bMean += bufferB[mind];
				count++;
			}
		}
		
		
		aMean /= (double)count;
		bMean /= (double)count;
		
		double var1 = 0;
		double var2 = 0;
		double covar = 0;
		double norm1, norm2;
		double val1, val2;
			
		for(int j=0;j<height;j++){
			ya = (j+offsetYa) * origWidth;
			yb = (j+offsetYb) * origWidth;
			for(int i=0;i<width;i++){
				ind = (i + offsetXa) + ya;
				mind = (i + offsetXb) + yb;
				val1 = bufferA[ind];
				val2 = bufferB[mind];
				norm1 = val1 - aMean;
				norm2 = val2 - bMean;
				covar += norm1*norm2;
				var1 += norm1*norm1;
				var2 += norm2*norm2;
			}
		}
		
		
		var1 /= (double)count;
		var2 /= (double)count;
		covar /= (double)count;
		double std1 = Math.sqrt(var1);
		double std2 = Math.sqrt(var2);
		
		R = covar / (std1*std2);
		
		return R;
	}
	
	private double corr(int ptX, int ptY, int ptZ){
		
		double R = 0;
		int width, height, depth;
		int offsetXa, offsetYa, offsetZa;
		int offsetXb, offsetYb, offsetZb;
		
		if(ptX >= 0){
			offsetXa = ptX;
			offsetXb = 0;
			width = this.width - ptX;
		}
		else{
			offsetXa = 0;
			offsetXb = - ptX;
			width = this.width + ptX;
		}
		if(ptY >= 0){
			offsetYa = ptY;
			offsetYb = 0;
			height = this.height - ptY;
		}
		else{
			offsetYa = 0;
			offsetYb = - ptY;
			height = this.width + ptY;
		}
		if(ptZ >= 0){
			offsetZa = ptZ;
			offsetZb = 0;
			depth = this.depth - ptZ;
		}
		else{
			offsetZa = 0;
			offsetZb = - ptZ;
			depth = this.depth + ptZ;
		}
		
		if (100*width*depth*height / sliceSize > overlap)
			return Float.NEGATIVE_INFINITY;
		
		int ind, mind;
		int ya, yb;
		int za, zb;
		double aMean = 0;
		double bMean = 0;
		int count = 0;
		
		for(int k=0;k<depth;k++){
			za = (k+offsetZa) * length;
			zb = (k+offsetZb) * length;
			for(int j=0;j<height;j++){
				ya = (j+offsetYa) * this.width;
				yb = (j+offsetYb) * this.width;
				for(int i=0;i<width;i++){
					ind = (i + offsetXa) + ya + za;
					mind = (i + offsetXb) + yb + zb;
					aMean += bufferA[ind];
					bMean += bufferB[mind];
					count++;
				}
			}
		}
		
		aMean /= (double)count;
		bMean /= (double)count;
		
		double var1 = 0;
		double var2 = 0;
		double covar = 0;
		double norm1, norm2;
		double val1, val2;
		
		for(int k=0;k<depth;k++){
			za = (k+offsetZa) * length;
			zb = (k+offsetZb) * length;
			for(int j=0;j<height;j++){
				ya = (j+offsetYa) * this.width;
				yb = (j+offsetYb) * this.width;
				for(int i=0;i<width;i++){
					ind = (i + offsetXa) + ya + za;
					mind = (i + offsetXb) + yb + zb;
					val1 = bufferA[ind];
					val2 = bufferB[mind];
					norm1 = val1 - aMean;
					norm2 = val2 - bMean;
					covar += norm1*norm2;
					var1 += norm1*norm1;
					var2 += norm2*norm2;
				}
			}
		}
		
		var1 /= (double)count;
		var2 /= (double)count;
		covar /= (double)count;
		double std1 = Math.sqrt(var1);
		double std2 = Math.sqrt(var2);
		
		R = covar / (std1*std2);
		
		return R;
	}
	
	private boolean isLocalMax(int x, int y){
		
		float center = corrBuffer[x + y*width];
		int row;
		for(int ny = y-1;ny<=y+1;ny++){
			row = ny*width;
			if(ny<0 || ny>=height) continue;
			for(int nx = x-1;nx<=x+1;nx++){
				if(nx<0 || nx>=width) continue;
				if(corrBuffer[nx + row] > center)
					return false;
			}
		}
		return true;
	}
	
	private boolean isLocalMax(int x, int y, int z){
		
		float center = corrBuffer[x + y*width + z*sliceSize];
		int row, slice;
		for(int nz = z-1;nz<=z+1;nz++){
			if(nz<0 || nz >= depth) continue;
			slice = nz*sliceSize;
			for(int ny = y-1;ny<=y+1;ny++){
				row = ny*width;
				if(ny<0 || ny>=height) continue;
				for(int nx = x-1;nx<=x+1;nx++){
					if(nx<0 || nx>=width) continue;
					if(corrBuffer[nx + row + slice] > center)
						return false;
				}
			}
		}
		return true;
	}
	
	private ModelImage padPeriodicImage(ModelImage im){
		
		int x, y, nx, ny, dx, dy;
		int padX, padY;
		x = original[0];
		y = original[1];
		
		nx = (int) Math.ceil(Math.log(x)/Math.log(2));
		ny = (int) Math.ceil(Math.log(y)/Math.log(2));
		
		nx = (int)Math.pow(2, nx);
		ny = (int)Math.pow(2, ny);
		
		if(nx == x && ny == y) return im;
		dx = nx - x;
		dy = ny - y;
		padX = dx/2;
		padY = dy/2;
		int[] newExtents = new int[]{nx, ny};
		ModelImage newImage = new ModelImage(im.getType(), newExtents, im.getImageName()+ "_padded");
		int[] newBuffer = new int[nx*ny];
		int[] oldBuffer = new int[x*y];
		try {
			im.exportData(0, x*y, oldBuffer);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		/*double a = 1.5;
		
		double weightsX[] = new double[x];
		double weightsY[] = new double[y];
		
		for(int i = 0; i<x;i++){
			double relPos = (double) i / (double)(x-1);
			
			if (relPos <= 0.5)
				weightsX[i] = 1.0 - (1.0 / (Math.pow(a, (relPos * 2))));
			else
				weightsX[i] = 1.0 - (1.0 / (Math.pow(a, ((1 - relPos) * 2))));
		}
		
		for(int i = 0; i<y;i++){
			double relPos = (double) i / (double)(y-1);
			
			if (relPos <= 0.5)
				weightsY[i] = 1.0 - (1.0 / (Math.pow(a, (relPos * 2))));
			else
				weightsY[i] = 1.0 - (1.0 / (Math.pow(a, ((1 - relPos) * 2))));
		}*/
		
		int shiftedY, shiftedX;
		int ix, iy;
		
		for(int j=0;j<y;j++){
			shiftedY = j + padY;
			for(int i=0;i<x;i++){
				shiftedX = i + padX;
				newBuffer[shiftedY*nx + shiftedX] = oldBuffer[i+j*x];
				//newBuffer[shiftedY*nx + shiftedX] = (int) (weightsX[i]*weightsY[j]*oldBuffer[i+j*x]);
			}
		}
		/*for(int j=-padY;j<y+padY;j++){
			shiftedY = j+padY;
			if(j<0) iy = -j;
			else if(j>=y) iy = 2*y-j-1;
			else iy = j;
			for(int i=-padX;i<x+padX;i++){
				shiftedX = i+padX;
				if(i<0) ix = -i;
				else if(i>=x) ix = 2*x-i-1;
				else ix = i;
				newBuffer[shiftedX + shiftedY*nx] = oldBuffer[ix + iy*original[0]];
			}
		}*/
		
		try{
			newImage.importData(0, newBuffer, true);
		}catch(IOException e){
			e.printStackTrace();
		}
		
		//new ViewJFrameImage(newImage);
	
		return newImage;
	}
	
	private void cropBack(){
		
		float[] newBuffer = new float[original[0]*original[1]];
		int offsetX = (width - original[0])/2;
		int offsetY = (height - original[1])/2;
		int y, ny;
		for(int i=0;i<original[1];i++){
			y = (i+offsetY)*width;
			ny = i*original[1];
			System.arraycopy(corrBuffer, offsetX + y, newBuffer, ny, original[0]);
		}
		
		corrBuffer = newBuffer;
		extents = original;
		width = extents[0];
		height = extents[1];
		length = width*height;
	}
	
	private class MaxCoord implements Comparable<MaxCoord>{
		
		private int x;
		
		private int y;
		
		private int z;
		
		private float corr;
		
		private MaxCoord(int ptX, int ptY, int ptZ, float val){
			x = ptX;
			y = ptY;
			z = ptZ;
			corr = val;
		}

		@Override
		public int compareTo(MaxCoord m) {
			if(this.corr > m.corr) return 1;
			else if(this.corr < m.corr) return -1;
			else return 0;
		}
		
	}

}
