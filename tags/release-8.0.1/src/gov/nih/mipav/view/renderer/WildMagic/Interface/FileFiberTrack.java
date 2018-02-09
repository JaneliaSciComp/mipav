package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.LEFileReader;
import gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork.LEFileWriter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class FileFiberTrack {


	public static Vector<VOIContour> loadDTIStudioPolyline( String fileName )
	{
		Vector<VOIContour> akSelected = null;
		try {
			LEFileReader fp = new LEFileReader(fileName);

			//Read HEADER
			for ( int i = 0; i < 8; i++ )
			{
				char c =(char)fp.readByte();
			}

			int nFibers = fp.readInt();	//Number of Fibers
			int FiberLenMax = fp.readInt(); 	
			float FiberLenMean =fp.readFloat();	
			int dimX = fp.readInt();			
			int dimY = fp.readInt();		
			int dimZ = fp.readInt();		
			float resX = fp.readFloat();	
			float resY = fp.readFloat();	
			float resZ = fp.readFloat();	

			int so= fp.readInt();	
			String slice_ori;
			switch(so){
			case 0:
				slice_ori = "Coronal";
				break;
			case 1:
				slice_ori = "Axial";
				break;
			case 2:
				slice_ori = "Sagittal";
				break;
			default:
				slice_ori = "Unknown";
				break;
			}

			so=fp.readInt();				
			String slice_seq;
			switch(so) {
			case 0: slice_seq = "Positive";
			break;
			case 1: slice_seq = "Negative";
			break;
			default: slice_seq = "Unknown";
			break;
			}

			fp.seek(128);

			akSelected = new Vector<VOIContour>();

			//WRITE FIBERS
			for(int i=0;i<nFibers;i++){

				int fiberLength = fp.readInt();	//fiber length
				int temp = fp.readByte();			//cReserved

				int r = fp.readByte();
				int g = fp.readByte();
				int b = fp.readByte();

				temp = fp.readInt();			//nSelectFiberStartPoint
				fiberLength = fp.readInt();	//nSelec

				//WRITE EVERY POINT IN THE FIBER 
				VOIContour kContour = new VOIContour(false);
				for (int j=0;j<fiberLength;j++){
					kContour.add( new Vector3f( fp.readFloat(), fp.readFloat(), fp.readFloat() ) );
				}
				akSelected.add( kContour );
			}




		} catch (FileNotFoundException e) {
			return null;
		} catch(IOException e){
			return null;				
		}
		return akSelected;
	}

	/**
	 * Gets the stats.
	 * 
	 * @param allFibers the all fibers
	 * 
	 * @return the stats
	 */
	private static float[] getStats(Vector<VOIContour> allFibers){
		int max = Integer.MIN_VALUE;
		int min = Integer.MAX_VALUE;
		int mean = 0;

		for(int i=0; i<allFibers.size(); i++){

			int n = allFibers.elementAt(i).size();
			if(n<min){
				min=n;
			}else if(n>max){
				max = n;
			}
			mean = mean+n;
		}
		if(allFibers.size()>0){
			mean = mean/allFibers.size();
		}
		float[] stats = {min, max, mean};
		return stats;
	}

	public static void writeDTIStudioPolyline( String fileName, Vector<VOIContour> akSelected, int[] extents, float[] resolutions )
	{
		try {
			LEFileWriter fp = new LEFileWriter(fileName);

			//WRITE HEADER
			//DTIStudioHDR hdr = new DTIStudioHDR(); 
			fp.writeBytes("FiberDat"); 

			fp.writeInt(akSelected.size());	//Number of Fibers

			float[] stats = getStats(akSelected);

			fp.writeInt((int)stats[1]);		//FLM
			fp.writeFloat(stats[2]);	//FLMn

			//bos.writeFloat(0);

			fp.writeInt(extents[0]);
			fp.writeInt(extents[1]);
			fp.writeInt(extents[2]);

			fp.writeFloat(resolutions[0]);
			fp.writeFloat(resolutions[1]);
			fp.writeFloat(resolutions[2]);

			fp.writeInt(1);
			fp.writeInt(0);

			fp.seek(128);


			//WRITE FIBERS
			for(int i=0;i<akSelected.size();i++){

				VOIContour f = akSelected.elementAt(i);		//GET THE iTH FIBER

				fp.writeInt(f.size());	//fiber length
				fp.writeByte(0);			//cReserved

				//HOW TO COME UP WITH COLORS?
				//everything red unless specified
				fp.writeByte(255);				//Color - Red
				fp.writeByte(0);				//Color - Green
				fp.writeByte(0);				//Color - Blue

				fp.writeInt(0);			//nSelectFiberStartPoint
				fp.writeInt(f.size());	//nSelec

				//WRITE EVERY POINT IN THE FIBER 
				for (int j=0;j<f.size();j++){
					fp.writeFloat(f.elementAt(j).X);
					fp.writeFloat(f.elementAt(j).Y);
					fp.writeFloat(f.elementAt(j).Z);
				}
			}
			fp.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	public static Vector<VOIContour> loadVTKLegacyPolyline(String fileName) {
		RandomAccessFile kIn;
		File file = new File(fileName);
		try {
			kIn = new RandomAccessFile(file, "r");
		} catch (IOException e) {
			return null;
		}

		Vector<VOIContour> akSelected = null;

		//System.out.println(fileName);

		StringBuffer buff = new StringBuffer();
		try {
			String str;
			// Read file as string
			while ((str = kIn.readLine()) != null) {
				buff.append(str+"\n");
			}
		} catch (Exception e) {
			System.err.println("Error occured while reading parameter file:\n"+e.getMessage());
			e.printStackTrace();
			return null;
		}
		Pattern header=Pattern.compile("POINTS\\s\\d+\\sfloat");
		Matcher m=header.matcher(buff);
		int vertexCount=0;

		Vector<Vector3f> vertices = new Vector<Vector3f>();
		if(m.find()){
			String head=buff.substring(m.start(),m.end());
			String[] vals=head.split("\\D+");
			if(vals.length>0){
				try {
					vertexCount=Integer.parseInt(vals[vals.length-1]);
				} catch(NumberFormatException e){
					System.err.println("CANNOT DETERMINE VERTEX COUNT");
					return null;
				}
			}

			//System.out.println("vertex count is " + vertexCount);
			//System.out.println(m.end());
			//System.out.println(buff.length());
			String[] strs=buff.substring(m.end(),buff.length()).split("\\s+",vertexCount*3+2);
			//System.out.println(strs[0]);
			//System.out.println(strs[1]);
			for(int i=1;i<strs.length-1;i+=3){
				try {
					vertices.add( new Vector3f (
							Float.parseFloat(strs[i]),
							Float.parseFloat(strs[i+1]),
							Float.parseFloat(strs[i+2]) ) );
					//System.out.println(i/3+")"+p);
				} catch(NumberFormatException e){
					System.err.println("CANNOT FORMAT VERTS");
					return null;
				}
			}
		} else {
			return null;
		}

		int nLines = 0;
		header=Pattern.compile("LINES\\s+\\d+\\s+\\d+");
		m=header.matcher(buff);
		if(m.find()){
			String head=buff.substring(m.start(),m.end());
			String[] vals=head.split("\\D+");
			if(vals.length>1){
				try {
					nLines=Integer.parseInt(vals[1]);
				} catch(NumberFormatException e){
					System.err.println("CANNOT DETERMINE NUMBER LINES COUNT");
					return null;
				}
			}
			
			akSelected = new Vector<VOIContour>();
			String[] strs=buff.substring(m.end(),buff.length()).split("\\n+");	
			for(int i=1;i<=nLines;i++) {			
				try {
					String[] indices = strs[i].split("\\s+");
					int nPoints = Integer.parseInt(indices[0]);
					VOIContour kTrack = new VOIContour(false);
					for ( int j = 1; j < nPoints; j++ )
					{
						kTrack.add( vertices.elementAt(Integer.parseInt(indices[j])) );
					}
					akSelected.add( kTrack );
				} catch(NumberFormatException e){
					System.err.println("CANNOT FORMAT INDICES");
					return null;
				}
			}
		} else {
			return null;
		}
		return akSelected;
	}


	public static void writeVTKLegacyPolyline(String fileName, Vector<VOIContour> akSelected) {

		try {
			PrintWriter kOut = new PrintWriter(new FileWriter(fileName));
			kOut.println("# vtk DataFile Version 3.0");
			kOut.println("");
			kOut.println("ASCII");
			kOut.println("DATASET POLYDATA");
			int pointCount = 0;
			for ( int i = 0; i < akSelected.size(); i++ )
			{
				pointCount += akSelected.elementAt(i).size();
			}
			kOut.println("POINTS "+pointCount+" float");
			Vector3f p=new Vector3f();
			String tmp;
			for ( int i = 0; i < akSelected.size(); i++ )
			{
				for ( int j = 0; j < akSelected.elementAt(i).size(); j++ )
				{
					p = akSelected.elementAt(i).elementAt(j);
					tmp=String.format("%.5f %.5f %.5f\n", p.X,p.Y,p.Z);
					kOut.print(tmp);
				}
			}

			int numLines = akSelected.size();
			kOut.println("LINES " + numLines + " " + pointCount);
			int index = 0;
			for ( int i = 0; i < akSelected.size(); i++ )
			{
				tmp=String.format("%d ", akSelected.elementAt(i).size());
				kOut.print(tmp);
				for ( int j = 0; j < akSelected.elementAt(i).size(); j++ )
				{
					tmp=String.format("%d ", index++);
					kOut.print(tmp);
				}
				kOut.print("\n");
			}
			kOut.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}



	public static File[] openFiles(boolean bLoad) {

		// file dialog to select surface mesh files (*.sur)
		JFileChooser chooser = new JFileChooser();
		chooser.setMultiSelectionEnabled(bLoad);
		chooser.resetChoosableFileFilters();
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FIBER));

		if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
			chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
		} else {
			chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
		}

		int returnVal;

		if (bLoad) {
			returnVal = chooser.showOpenDialog(null);
		} else {
			returnVal = chooser.showSaveDialog(null);
		}

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
					File.separatorChar);

			if (bLoad) {
				File[] files = chooser.getSelectedFiles();

				return files;
			} 
			File[] files = new File[1];
			files[0] = chooser.getSelectedFile();
			return files;
		}

		return null;
	}


}
