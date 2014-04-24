import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.BitSet;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.view.MipavUtil;

public class PlugInAlgorithmSaveTraceAsAVI extends AlgorithmBase {
	
	private ArrayList<File> traceList;
	
	private float slicesPerSecond;
	
	private boolean isSWC;
	
	private ModelImage saveImage;
	
	private int width;
	
	private int height;
	
	private int length;
	
	private int depth;
	
	private String name;

	public PlugInAlgorithmSaveTraceAsAVI(ArrayList<File> files, float freq, boolean swc, String aviName){
		super();
		
		traceList = files;
		slicesPerSecond = freq;
		isSWC = swc;
		name = name;
	}
	
	@Override
	public void runAlgorithm() {
		
		depth = traceList.size();
		try{
			if(isSWC){
				readSWC();
			} else {
				readImages();
			}
		} catch (IOException e){
			MipavUtil.displayError("Unable to read files");
			e.printStackTrace();
			return;
		}
		
		//ViewJFrameImage frame = new ViewJFrameImage(saveImage);
		//frame.setVisible(false);
		
		int microSec = (int)(1000000.0f / slicesPerSecond);
		
		//saveImage.saveImage(parent, "trace_movie", FileUtility.AVI, false);
		
		FileWriteOptions options = new FileWriteOptions(false);
		
		File tempFile = traceList.get(0);
		String parent = tempFile.getParent() + File.separator;
		options.setBeginSlice(0);
        options.setEndSlice(depth - 1);
        
        options.setRunningInSeparateThread(false);
        options.setFileType(FileUtility.AVI);
        options.setIsAVI(true);
        options.setAVICompression(0);
        options.setMicroSecPerFrame(microSec);
        options.setFileName(name + ".avi");
        options.setFileDirectory(parent);
        options.setLUTa(new ModelLUT());
        options.setPaintBitmap(new BitSet(length*depth));
        //options.setLUTa(frame.getLUTa());
        //options.setRGBTa(frame.getRGBTA());
        //options.setPaintBitmap(frame.getComponentImage().getPaintBitmap());
        options.setOptionsSet(true);
        
        FileIO imWriter = new FileIO();
        imWriter.writeImage(saveImage, options);
        
        saveImage.disposeLocal();

        setCompleted(true);
	}
	
	private BitSet bresenham(int x0, int y0, int x1, int y1, int z){
		
		BitSet line = new BitSet(length);
		int dx = Math.abs(x1-x0);
		int dy = Math.abs(y1-y0);
		int sx, sy;
		int err, e2;
		if(x0 < x1)
			sx = 1;
		else sx = -1;
		if(y0 < y1)
			sy = 1;
		else sy = -1;
		err = dx - dy;
		
		int i;
		
		while(true){
			i = x0 + y0*width;
			line.set(i);
			
			if(x0 == x1 && y0 == y1) break;
			e2 = 2*err;
			if(e2 > -dy){
				err -= dy;
				x0 += sx;
			}
			if(e2 < dx){
				err += dx;
				y0 += sy;
			}
		}
		
		return line;
	}
	
	private void readImages() throws IOException{
		
		ModelImage tempImage;
		FileIO imReader = new FileIO();
		tempImage = imReader.readImage(traceList.get(0).toString());
		int[] extents = tempImage.getExtents();
		width = extents[0];
		height = extents[1];
		length = width*height;
		extents = new int[]{width, height, depth};
		saveImage = new ModelImage(ModelImage.USHORT, extents, "Trace Stack");
		
		BitSet trace = new BitSet(length);
		int[] buffer = new int[255];
		tempImage.exportData(0, length, trace);
		saveImage.importData(0, trace, true);
		tempImage.disposeLocal();
		
		for(int k=1;k<depth;k++){
			trace = new BitSet(length);
			tempImage = imReader.readImage(traceList.get(k).toString());
			tempImage.exportData(0, length, trace);
			for(int i=trace.nextSetBit(0);i>=0;i=trace.nextSetBit(i+1)){
				buffer[i] = 255;
			}
			saveImage.importData(k*length, trace, true);
			tempImage.disposeLocal();
		}
	}
	
	private void readSWC() throws IOException{
		//Read first SWC to get width/height
		BufferedReader input = new BufferedReader(new FileReader(traceList.get(0)));
		String line = null; 
		while(( line = input.readLine()) != null){
			if(line.startsWith("# Width")){
				String[] parts = line.split(" ");
				width = Integer.valueOf(parts[2]);
			}
			else if(line.startsWith("# Height")){
				String[] parts = line.split(" ");
				height = Integer.valueOf(parts[2]);
			}
		}

		input.close();

		length = width*height;
		int[] extents = new int[]{width, height, depth};

		saveImage = new ModelImage(ModelImage.USHORT, extents, "Trace Stack");

		for(int k=0;k<depth;k++){
			BitSet traces = new BitSet(length);
			int[] buffer = new int[length];
			ArrayList<String[]> points = new ArrayList<String[]>();
			input = new BufferedReader(new FileReader(traceList.get(k)));
			while(( line = input.readLine()) != null){
				if(!line.startsWith("#"))
					points.add(line.split(" "));
			}

			int linkedTo;
			int x0, x1, y0, y1;
			String num;
			String[] lineArray;
			for(int i=points.size()-1;i>0;i--){
				lineArray = points.get(i);
				num = lineArray[2];
				num = num.substring(0, num.indexOf("."));
				x0 = Integer.parseInt(num);
				num = lineArray[3];
				num = num.substring(0, num.indexOf("."));
				y0 = height - Integer.parseInt(num);

				//Make the various line VOIs
				linkedTo = Integer.parseInt(lineArray[6]);

				lineArray = points.get(linkedTo - 1);
				num = lineArray[2];
				num = num.substring(0, num.indexOf("."));
				x1 = Integer.parseInt(num);
				num = lineArray[3];
				num = num.substring(0, num.indexOf("."));
				y1 = height - Integer.parseInt(num);

				traces.or(bresenham(x0, y0, x1, y1, k));
			}
			for(int i=traces.nextSetBit(0);i>=0;i=traces.nextSetBit(i+1)){
				buffer[i] = 255;
			}
			saveImage.importData(k*length, buffer, true);
		}
	}
}
