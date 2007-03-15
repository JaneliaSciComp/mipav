import java.awt.Color;
import java.awt.Cursor;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileDicom;
import gov.nih.mipav.model.file.FileDicomTag;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewFileTreeNode;
import gov.nih.mipav.view.ViewImageFileFilter;

public class PlugInAlgorithmDTISortingProcess extends AlgorithmBase {

	/** This is the full path of the study **/
	private String studyPath;

	/** This is the dir name of the study **/
	private String studyName;

	/** This is the full path to the gradient file **/
	private String gradientFilePath;

	/** This is the list for all the files in the study dir**/
	private Vector fileInfoVector;

	/** This is an ordered list of files per series number **/
	private TreeSet seriesFileInfoTreeSet;

	/** This is an ordered map of series number and  seriesFileInfoTreeSet**/
	private TreeMap seriesFileInfoTreeMap;

	/** This is the image filter needed to select the correct dicom images **/
	private ViewImageFileFilter imageFilter;

	/** this is the gradient matrix that is populated when reading the gradient file **/
	private float[][] direction;

	/** flag indicating if method was successful **/
	private boolean success = true;
	
	/** this is the number of image slices per volume **/
	private int numSlicesPerVolume;
	
	/** this is the number of gradient quantities that is obtained by reading first line of gradient file **/
	private int nim;

	/** this is an array of b-values that for each volume**/
	private ArrayList bValuesArrayList = new ArrayList();
	
	/** TextArea of main dialogfor text output **/
	private JTextArea outputTextArea;
	
	/** boolean if proc dir was created **/
	private boolean isProcDirCreated = false;
	
	/** total num of volumes in study **/
	private int totalNumVolumes = 0;
	


	/**
	 * constructor
	 * @param studyPath
	 * @param gradientFilePath
	 */
	public PlugInAlgorithmDTISortingProcess(String studyPath, String studyName, String gradientFilePath, JTextArea outputTextArea) {
		this.studyPath = studyPath;
		this.studyName = studyName;
		this.gradientFilePath = gradientFilePath;
		this.outputTextArea = outputTextArea;
		fileInfoVector = new Vector();
	}

	
	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		Preferences.debug("** Beginning Algorithm \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("** Beginning Algorithm \n");

		Preferences.debug("* The study path is " + studyPath + " \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* The study path is " + studyPath + " \n");

		// first create a File object based upon the study path
		File studyPathRoot = new File(studyPath);

		// parse the directory and populate the fileInfoVector
		Preferences.debug("* Parsing... \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Parsing... \n");
		success = parse(studyPathRoot);
		if (success == false) {
			finalize();
			return;
		}

		Preferences.debug("* Number of images in study dir is " + fileInfoVector.size() + " \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Number of images in study dir is " + fileInfoVector.size() + " \n");
		
		// now lets separate the fileInfoVector depending on series # and
		// then populate the seriesFileInfoVectorMap
		seriesFileInfoTreeMap = new TreeMap();

		for (int i = 0; i < fileInfoVector.size(); i++) {
			FileInfoDicom fileInfoDicom = (FileInfoDicom) fileInfoVector.elementAt(i);
			String seriesNumber_String = ((String) fileInfoDicom.getValue("0020,0011")).trim();
			if (seriesNumber_String == null || seriesNumber_String.equals("")) {
				seriesNumber_String = "0";
			}
			Integer seriesNumber = new Integer(seriesNumber_String);
			if (seriesFileInfoTreeMap.get(seriesNumber) == null) {
				seriesFileInfoTreeSet = new TreeSet(new InstanceNumberComparator());
				seriesFileInfoTreeSet.add(fileInfoDicom);
				seriesFileInfoTreeMap.put(seriesNumber, seriesFileInfoTreeSet);
			} else {
				seriesFileInfoTreeSet = (TreeSet) seriesFileInfoTreeMap.get(seriesNumber);
				seriesFileInfoTreeSet.add(fileInfoDicom);
				seriesFileInfoTreeMap.put(seriesNumber, seriesFileInfoTreeSet);
			}
		}

		
		

		//create proc dir
		Preferences.debug("* Creating proc dir \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Creating proc dir \n");
		success = createProcDir();
		if (success == false) {
			finalize();
			return;
		}
		
		// create path file
		Preferences.debug("* Creating path file \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Creating path file \n");
		success = createPathFile();
		if (success == false) {
			finalize();
			return;
		}
		
		
		//read gradient file
		Preferences.debug("* Reading gradient file \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Reading gradient file \n");
		success = readGradientFile();
		if (success == false) {
			finalize();
			return;
		}
		
		//create list file
		Preferences.debug("* Creating list file \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Creating list file \n");
		success = createListFile();
		if (success == false) {
			finalize();
			return;
		}
		
		//obtain b-values
		Preferences.debug("* Obtaining b-values \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Obtaining b-values \n");
		success = obtainBValues();
		if (success == false) {
			finalize();
			return;
		}
		
		
		//create b matrix file
		Preferences.debug("* Creating b-matrix file \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("* Creating b-matrix file \n");
		success = createBMatrixFile();
		if (success == false) {
			finalize();
			return;
		}

		Preferences.debug("** Ending Algorithm \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("** Ending Algorithm \n");
		
		finalize();
	}
	
	
	
	
	
	/**
	 * this method parses the study dir and populates the fileInfoVector
	 * @param file
	 * @return
	 */
	public boolean parse(File file) {

		imageFilter = new ViewImageFileFilter(new String[] { "dcm", "DCM", "ima", "IMA" });

		File[] children = file.listFiles();
		FileDicom imageFile = null;

		try {

			boolean success = false;
			for (int i = 0; i < children.length; i++) {
				if (isThreadStopped()) {
		            return false;
		        }
				if (children[i].isDirectory()) {
					parse(children[i]);
				} else if (!children[i].isDirectory()) {

					try {

						if (imageFile == null) {
							imageFile = new FileDicom(children[i].getName(), children[i].getParent() + File.separatorChar);
						} else {
							imageFile.setFileName(children[i]);
						}

						if (imageFilter.accept(children[i])) {
							success = imageFile.readParserHeader();
						} else if (imageFile.isDICOM()) {
							success = imageFile.readParserHeader();
						} else {
							continue;
						}
					} catch (IOException error) {
						outputTextArea.append(error.toString());
						outputTextArea.append("!!! Unable to read file to parse.....exiting algorithm \n");
						error.printStackTrace();
						return false;
					}

					FileInfoDicom fileInfo = (FileInfoDicom) imageFile.getFileInfo();

					fileInfoVector.addElement(fileInfo);

				}
			}
		} catch (Exception err) {
			err.printStackTrace();
			Preferences.debug(err.toString() + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("! ERROR: Unable parse file.....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(err.toString() + "\n");
			outputTextArea.append("! ERROR: Unable parse file.....exiting algorithm \n");
			return false;
		}
		if(fileInfoVector.size() == 0) {
			return false;
		} else {
			return true;
		}
	}
	
	
	
	
	/**
	 * this method creates the proc dir in which the list file, path file, and b-matrix file go
	 * @return boolean success
	 */
	public boolean createProcDir() {
		// create parallel proc dir to study path that will hold list file, path
		// file, and b matrix file
		File procDir = new File(studyPath + "_proc");
		if (!procDir.isDirectory()) {
			boolean success = new File(studyPath + "_proc").mkdir();
			if (!success) {
				Preferences.debug("! ERROR: Creation of proc directory failed....exiting algorithm \n",Preferences.DEBUG_ALGORITHM);
				outputTextArea.append("! ERROR: Creation of proc directory failed....exiting algorithm \n");
				return false;
			}
		} else {
			// we should delete all the files in the procDir if there are any
			File[] listFiles = procDir.listFiles();
			if (listFiles.length > 0) {
				for (int i = 0; i < listFiles.length; i++) {
					listFiles[i].delete();
				}
			}
		}
		
		Preferences.debug(" - proc dir created : " + studyPath + "_proc \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append(" - proc dir created : " + studyPath + "_proc \n");
		isProcDirCreated = true;
		return true;
	}

	
	
	
	/**
	 * This method creates the path file
	 * @return boolean success
	 */
	public boolean createPathFile() {

		try {
			File pathFile = new File(studyPath + "_proc" + File.separator + studyName + ".path");
			FileOutputStream outputStream = new FileOutputStream(pathFile);
			PrintStream printStream = new PrintStream(outputStream);
			Set ketSet = seriesFileInfoTreeMap.keySet();
			Iterator iter = ketSet.iterator();
			ArrayList numSlicesCheckList = new ArrayList();
			while (iter.hasNext()) {
				TreeSet seriesFITS = (TreeSet) seriesFileInfoTreeMap.get(iter.next());
				int seriesFITSSize = seriesFITS.size();
				Iterator iter2 = seriesFITS.iterator();
				// lets get the first element and remember its imageSlice
				String imageSlice = ((String) (((FileInfoDicom) seriesFITS.first()).getValue("0020,1041"))).trim();

				// now we need to figure out how many slices are in each
				// vol...do this by
				// finding at what value the counter is when it is equal to the
				// first one since
				// the imageSlice wraps around...this represents the next
				// vol...so the num of slices
				// in each vol is 1 less that
				int counter = 1;
				int sliceNum = 0;
				while (iter2.hasNext()) {
					FileInfoDicom fid = (FileInfoDicom) iter2.next();
					String imgSlice = ((String) fid.getValue("0020,1041")).trim();
					//this is to just get total num volumes
					if (imgSlice.equals(imageSlice)) {
						totalNumVolumes = totalNumVolumes + 1;
					}
					//this is to compare the image slices...but we dont want to get the first one
					if (imgSlice.equals(imageSlice) && counter != 1) {
						
						numSlicesCheckList.add(new Integer(counter - 1 - sliceNum));
						sliceNum = counter - 1;
					}
					++counter;
				}
				//counter = counter - 1;
			}
			numSlicesPerVolume = ((Integer)numSlicesCheckList.get(0)).intValue();
			for(int i=1;i<numSlicesCheckList.size();i++) {
				if(((Integer)numSlicesCheckList.get(0)).intValue() != numSlicesPerVolume) {
					Preferences.debug("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
					outputTextArea.append("! ERROR: There are not equal number of image slices in all volumes of this study....exiting algorithm \n");
					return false;
				}
			}
			
			//ok....we have equal # of image slices in all volumes of the study
			Object[] keyArray = seriesFileInfoTreeMap.keySet().toArray();
			String relPath;
			for(int i=0;i<numSlicesPerVolume;i++) {
				for(int k=0;k<keyArray.length;k++) {
					Object[] fidArr = ((TreeSet) seriesFileInfoTreeMap.get(keyArray[k])).toArray();
					int numVols = fidArr.length / numSlicesPerVolume;
					String absPath = ((FileInfoDicom) fidArr[i]).getFileDirectory();
					relPath = new String(".." + File.separator + studyName +absPath.substring(absPath.lastIndexOf(studyName) + studyName.length(), absPath.length()));
					printStream.println(relPath + ((FileInfoDicom) fidArr[i]).getFileName());
					for (int j = 1; j < numVols; j++) {
						absPath = ((FileInfoDicom) fidArr[i + (numSlicesPerVolume * j)]).getFileDirectory();
						relPath = new String(".." + File.separator + studyName + absPath.substring(absPath.lastIndexOf(studyName) + studyName.length(), absPath.length()));
						printStream.println(relPath + ((FileInfoDicom) fidArr[i + (numSlicesPerVolume * j)]).getFileName());
					}
					
				}
			}
			outputStream.close();
			Preferences.debug(" - path file created : " + studyPath + "_proc" + File.separator + studyName + ".path" + " \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(" - path file created : " + studyPath + "_proc" + File.separator + studyName + ".path" + " \n");
			return true;
		}
		catch(Exception e) {
			Preferences.debug(e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("! ERROR: Creation of path file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(e.toString() + "\n");
			outputTextArea.append("! ERROR: Creation of path file failed....exiting algorithm \n");
			e.printStackTrace();
			return false;
		}
	}

	
	
	
	/**
	 * This method creates the list file
	 * @return boolean success
	 */
	public boolean createListFile() {
		// we can get the info for the list file from just 1 of the imageSlices'
		// FileInfoDicom
		FileInfoDicom fileInfoDicom = (FileInfoDicom) fileInfoVector.elementAt(0);

		Short originalColumns = (Short)(fileInfoDicom.getValue("0028,0011"));
		String originalColumsString = originalColumns.toString();

		Short originalRows = (Short) (fileInfoDicom.getValue("0028,0010"));
		String originalRowsString = originalRows.toString();

		String dir = ((String) (fileInfoDicom.getValue("0018,1312"))).trim();
		String phaseEncodeDirection = "";
		if (dir.equalsIgnoreCase("col")) {
			phaseEncodeDirection = "vertical";
		} else if (dir.equalsIgnoreCase("row")) {
			phaseEncodeDirection = "horizontal";
		}



		String sliceThickness = ((String) (fileInfoDicom.getValue("0018,0050"))).trim();
		float sliceTh = new Float(sliceThickness.trim()).intValue();

		String sliceGap = ((String) (fileInfoDicom.getValue("0018,0088"))).trim();
		float sliceGp = new Float(sliceGap.trim()).intValue();
		sliceGp = sliceGp - sliceTh;
		sliceGap = String.valueOf(sliceGp);
		
		String fieldOfView = (String) (fileInfoDicom.getValue("0018,1100"));
		if(fieldOfView == null || fieldOfView.trim().equals("")) {
			//get pixel space in x direction
			String xyPixelSpacingString = ((String) (fileInfoDicom.getValue("0028,0030"))).trim();
			int index = xyPixelSpacingString.indexOf("\\");
			String xPixelSpacingString = xyPixelSpacingString.substring(0, index);
			float xPixelSpacing = new Float(xPixelSpacingString).floatValue();
			float fieldOfViewFloat = xPixelSpacing * originalColumns.shortValue();
			fieldOfView = String.valueOf(fieldOfViewFloat);
		}
		
		

		//hard coded for now
		String imagePlane = "axial";

		//hard coded for now
		String rawImageFormat = "dicom";

		String pathFilename = studyName + ".path";

		String bmtrixFilename = studyName + ".BMTXT";
		
		String nimString = String.valueOf(nim);
		

		try {
			File listFile = new File(studyPath + "_proc" + File.separator + studyName + ".list");
			FileOutputStream outputStream = new FileOutputStream(listFile);
			PrintStream printStream = new PrintStream(outputStream);
			
			printStream.println("<!-- DTI initialization file -->");
			printStream.println("<!-- do not remove the above comment line -->");
			printStream.println();
			printStream.println("<!-- NUMBER OF COLUMNS -->");
			printStream.println("<original_columns>" + originalColumsString + "</original_columns>");
			printStream.println();
			printStream.println("<!-- NUMBER OF ROWS -->");
			printStream.println("<original_rows>" + originalRowsString + "</original_rows>");
			printStream.println();
			printStream.println("<!-- NUMBER OF SLICES -->");
			printStream.println("<slice>" + numSlicesPerVolume + "</slice>");
			printStream.println();
			printStream.println("<!-- NUMBER OF BMATRICES -->");
			printStream.println("<nim>" + nimString + "</nim>");
			printStream.println();
			printStream.println("<!-- ORIENTATION OF PHASE ENCODING (vertical, horizontal) -->");
			printStream.println("<phase_encode_direction>" + phaseEncodeDirection + "</phase_encode_direction>");
			printStream.println();
			printStream.println("<!-- HORIZONTAL FIELD OF VIEW (in mm) -->");
			printStream.println("<x_field_of_view>" + fieldOfView + "</x_field_of_view>");
			printStream.println();
			printStream.println("<!-- VERTICAL FIELD OF VIEW (in mm) -->");
			printStream.println("<y_field_of_view>" + fieldOfView + "</y_field_of_view>");
			printStream.println();
			printStream.println("<!-- FORMAT OF RAW IMAGES (integer, float, dicom, dummy) -->");
			printStream.println("<rawimageformat>" + rawImageFormat + "</rawimageformat>");
			printStream.println();
			printStream.println("<!-- NAME OF BMATRIX FILE -->");
			printStream.println("<bmatrixfile>" + bmtrixFilename + "</bmatrixfile>");
			printStream.println();
			printStream.println("<!-- GAP BETWEEN SLICES (in mm. Write 0 for contiguous slices) -->");
			printStream.println("<slice_gap>" + sliceGap + "</slice_gap>");
			printStream.println();
			printStream.println("<!-- SLICE THICKNESS (in mm) -->");
			printStream.println("<slice_thickness>" + sliceThickness + "</slice_thickness>");
			printStream.println();
			printStream.println("<!-- IMAGE PLANE (axial,coronal,sagittal) -->");
			printStream.println("<image_plane>" + imagePlane + "</image_plane>");
			printStream.println();
			printStream.println("<!-- NAME OF FILE CONTAINING PATH OF RAW IMAGES -->");
			printStream.println("<raw_image_path_filename>" + pathFilename + "</raw_image_path_filename>");

			outputStream.close();
		} catch (Exception e) {
			Preferences.debug(e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("! ERROR: Creation of list file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(e.toString() + "\n");
			outputTextArea.append("! ERROR: Creation of list file failed....exiting algorithm \n");
			return false;
		}

		
		Preferences.debug(" - list file created : " + studyPath + "_proc" + File.separator + studyName + ".list" + " \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append(" - list file created : " + studyPath + "_proc" + File.separator + studyName + ".list" + " \n");
		return true;
	}
	
	
	
	
	
	/**
	 * This method reads in the gradient file
	 * gradient file can be in 2 differetn ways....for eDTI, there will be a number
	 * on first line followed by correct number of gradients
	 * if it is DTI, there will be no number and just 7 linies of gradients....this
	 * needs to be duplicated as many times as there are series
	 * @return boolean success
	 */
	public boolean readGradientFile() {
		try {
			String str;
			FileInputStream fis = new FileInputStream(gradientFilePath);
			BufferedReader d = new BufferedReader(new InputStreamReader(fis));
			String firstLine = d.readLine();
			String[] firstLineSplits = firstLine.split(" ");
			ArrayList arrList = new ArrayList();
			ArrayList arrList2 = new ArrayList();
			for(int i=0;i<firstLineSplits.length;i++) {
				if(!(firstLineSplits[i].trim().equals(""))){
					arrList.add(new Float(firstLineSplits[i]));
				}
			}
			if(arrList.size() == 1) {
				//this means the first line is just 1 number...indicating eDTI grad file
				nim = ((Float)arrList.get(0)).intValue();
				direction = new float[nim][3];
				while((str=d.readLine()) != null) {
					str = str.trim();
					String[] arr = str.split(" ");
					for(int i=0;i<arr.length;i++) {
						if(!(arr[i].trim().equals(""))){
							arrList2.add(new Float(arr[i]));
						}
					}
				}

				
				for(int j=0,k=0;j<nim;j++,k=k+3) {
					direction[j][0] = ((Float)arrList2.get(k)).floatValue();
					direction[j][1] = ((Float)arrList2.get(k+1)).floatValue();
					direction[j][2] = ((Float)arrList2.get(k+2)).floatValue();
				}
			}
			else {
				//this means it is for reg DTI grad file
				nim = totalNumVolumes;
				direction = new float[nim][3];

				while((str=d.readLine()) != null) {
					str = str.trim();
					String[] arr = str.split(" ");
					for(int i=0;i<arr.length;i++) {
						if(!(arr[i].trim().equals(""))){
							arrList.add(new Float(arr[i]));
						}
					}
				}
				
				for(int j=0,k=0;j<7;j++,k=k+3) {
					direction[j][0] = ((Float)arrList.get(k)).floatValue();
					direction[j][1] = ((Float)arrList.get(k+1)).floatValue();
					direction[j][2] = ((Float)arrList.get(k+2)).floatValue();
				}
				
				
				//we got the first 7 lines for the direction already...so start on the 8th
				int counter2 = 0;
				for(int i=8;i<=totalNumVolumes;i++) {
					
					direction[i-1][0] = direction[counter2][0];
					direction[i-1][1] = direction[counter2][1];
					direction[i-1][2] = direction[counter2][2];
					if(i%7 == 0) {
						//reset counter
						counter2 = 0;
					} else {
						counter2++;
					}	
				}
			}
			fis.close();
		}
		catch(Exception e) {
			Preferences.debug(e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("! ERROR: reading of gradient file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(e.toString() + "\n");
			outputTextArea.append("! ERROR: reading of gradient file failed....exiting algorithm \n");
			return false;
		}
		
		return true;
	}
	
	
	/**
	 * This method obtains the b-values for each volume
	 * from either the public tag or the private tag
	 * @return
	 */
	public boolean obtainBValues() {
		//get b-values for each volume set
		//first check to see if it eDTI....this is done by checking to see if totalNumVolumes is 119 or 120
		//if it is, then get SeriesDescription....if it is GE (GE_eDTI), then get BValue from Private Tag
		//if it is Siemens (SS_eDTI), then get b-value from public tag...in which the b-value
		//comes after ep_b
		//if it is not eDTI:
		// first try and retrieve the b-value from the public tag(0018,0024) and
		//if thereg expression "ep_b" is present, get the number that follows it..
		//however, format might also be ep_b1234#0   So...get the number between ep_b and #...this is the b-value
		//*** this line is no longer used in the algorithm..i will just comment out for now until we are sure:  -> if ep_b not there then look for fake_gedti...if there then use 0 for vol1 and 1000 rest of vols
		//else, get the b-value from the private tag(0043,1039)....the b-value is the first number in the string
		//that is separated by commas
		Set ketSet = seriesFileInfoTreeMap.keySet();
		Iterator iter = ketSet.iterator();
		boolean isEDTI = false;
		if (totalNumVolumes == 119 || totalNumVolumes ==120) {
			isEDTI = true;
		}
		if(isEDTI) {
			Preferences.debug(" - eDTI data set b-values :\n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(" - eDTI data set b-values :\n");
			while (iter.hasNext()) {
				TreeSet seriesFITS = (TreeSet) seriesFileInfoTreeMap.get(iter.next());
				int seriesFITSSize = seriesFITS.size();
				int numVols = seriesFITSSize / numSlicesPerVolume;
				Object[] fidArr = seriesFITS.toArray();
				boolean isGE = false;
				boolean isSiemens = false;
				int edtiIndex = -1;
				Float bValue;
				TreeSet firstTS = (TreeSet)seriesFileInfoTreeMap.get(seriesFileInfoTreeMap.firstKey());
				FileInfoDicom firstFID = (FileInfoDicom)firstTS.first();
				String seriesDescLowerCase = ((String)firstFID.getValue("0008,103E")).toLowerCase();
				String ge_edti = "ge_edti";
				String ss_edti = "ss_edti";
				edtiIndex = seriesDescLowerCase.indexOf(ge_edti);
				if(edtiIndex != -1) {
					isGE = true;
				}
				edtiIndex = -1;
				edtiIndex = seriesDescLowerCase.indexOf(ss_edti);
				if(edtiIndex != -1) {
					isSiemens = true;
				}
				if(isGE) {
					String bValueLongString_privTag = "";
					String bValueString;
					for(int k = 0; k < numVols; k++) {
						try {
							if(((FileInfoDicom)fidArr[numSlicesPerVolume * k]).getValue("0043,1039") != null) {
								bValueLongString_privTag = (String)(((FileInfoDicom)fidArr[numSlicesPerVolume * k]).getValue("0043,1039"));
							}
						}
						catch(NullPointerException e) {
							Preferences.debug("! ERROR: The private tag info of 0043,1039 needs to be added to your dicom dictionary....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
							outputTextArea.append("! ERROR: The private tag info of 0043,1039 needs to be added to your dicom dictionary....exiting algorithm \n");
							return false;
						}
						if (bValueLongString_privTag != null && (!bValueLongString_privTag.trim().equals(""))) {	
							int index_privTag = bValueLongString_privTag.indexOf("\\");
							if(index_privTag != -1) {
								bValueString = (bValueLongString_privTag.substring(0, index_privTag)).trim();
								bValue = new Float(bValueString);
								bValuesArrayList.add(bValue);
								continue;
							}
							else {
								Preferences.debug("! ERROR: No b-value found for GE eDTI....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
								outputTextArea.append("! ERROR: No b-value found for GE eDTI....exiting algorithm \n");
								return false;
							}
						}
						else {
							Preferences.debug("! ERROR: No b-value found for GE eDTI....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
							outputTextArea.append("! ERROR: No b-value found for GE eDTI....exiting algorithm \n");
							return false;
						}	
					}
				}
				else if(isSiemens) {
					String bValueLongString_pubTag = "";
					String bValueString;
					int poundIndex = -1;
					for(int k = 0; k < numVols; k++) {
						bValueLongString_pubTag = (String)(((FileInfoDicom)fidArr[numSlicesPerVolume * k]).getValue("0018,0024"));
						int length = 0;
						int index = -1;
						if(bValueLongString_pubTag != null && (!bValueLongString_pubTag.trim().equals(""))) {
							String ep_b = "ep_b";
						    length = ep_b.length();
						    index = bValueLongString_pubTag.indexOf(ep_b);
						    poundIndex = bValueLongString_pubTag.indexOf('#');
						    if(index != -1) {
								index = index + length;
								if(poundIndex != -1 && poundIndex > index) {
									bValueString = (bValueLongString_pubTag.substring(index, poundIndex)).trim();
								}
								else {
									bValueString = (bValueLongString_pubTag.substring(index, bValueLongString_pubTag.length())).trim();
								}
	
							    bValue = new Float(bValueString);
								bValuesArrayList.add(bValue);
								continue;
							}
						    else {
								Preferences.debug("! ERROR: No b-value found for Siemens eDTI....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
								outputTextArea.append("! ERROR: No b-value found for Siemens eDTI....exiting algorithm \n");
								return false;
						    }
						}
					}
					
				}
				else {
					Preferences.debug("! ERROR: error in determing type of eDTI....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
					outputTextArea.append("! ERROR: error in determing type of eDTI....exiting algorithm \n");
					return false;
				}
			}
		}
		else {
			Preferences.debug(" - DTI data set b-values :\n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(" - DTI data set b-values :\n");
			while (iter.hasNext()) {
				TreeSet seriesFITS = (TreeSet) seriesFileInfoTreeMap.get(iter.next());
				int seriesFITSSize = seriesFITS.size();
				int numVols = seriesFITSSize / numSlicesPerVolume;
				Object[] fidArr = seriesFITS.toArray();
				String bValueLongString_privTag = "";
				String bValueLongString_pubTag = "";
				String bValueString;
				Float bValue;
				int poundIndex = -1;
				for(int k = 0; k < numVols; k++) {
					bValueLongString_pubTag = (String)(((FileInfoDicom)fidArr[numSlicesPerVolume * k]).getValue("0018,0024"));
					int length = 0;
					int index = -1;
					if(bValueLongString_pubTag != null && (!bValueLongString_pubTag.trim().equals(""))) {
						String ep_b = "ep_b";
					    length = ep_b.length();
					    index = bValueLongString_pubTag.indexOf(ep_b);
					    poundIndex = bValueLongString_pubTag.indexOf('#');
					    if(index != -1) {
							index = index + length;
							if(poundIndex != -1 && poundIndex > index) {
								bValueString = (bValueLongString_pubTag.substring(index, poundIndex)).trim();
							}
							else {
								bValueString = (bValueLongString_pubTag.substring(index, bValueLongString_pubTag.length())).trim();
							}

						    bValue = new Float(bValueString);
							bValuesArrayList.add(bValue);
							continue;
						}/*
						else {
							String fake_gedti = "fake_gedti";
							index = bValueLongString_pubTag.indexOf(fake_gedti);	
						}
						if(index != -1) {
							if(k == 0) {
								bValuesArrayList.add(new Float(0));
								continue;
							}
							else {
								bValuesArrayList.add(new Float(1000));
								continue;
							}
						}*/
					}
					if(index == -1) {
						try {
							if(((FileInfoDicom)fidArr[numSlicesPerVolume * k]).getValue("0043,1039") != null) {
								bValueLongString_privTag = (String)(((FileInfoDicom)fidArr[numSlicesPerVolume * k]).getValue("0043,1039"));
							}
						}
						catch(NullPointerException e) {
							Preferences.debug("! ERROR: The private tag info of 0043,1039 needs to be added to your dicom dictionary....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
							outputTextArea.append("! ERROR: The private tag info of 0043,1039 needs to be added to your dicom dictionary....exiting algorithm \n");
							return false;
						}
						if (bValueLongString_privTag != null && (!bValueLongString_privTag.trim().equals(""))) {	
							int index_privTag = bValueLongString_privTag.indexOf("\\");
							if(index_privTag != -1) {
								bValueString = (bValueLongString_privTag.substring(0, index_privTag)).trim();
								bValue = new Float(bValueString);
								bValuesArrayList.add(bValue);
								continue;
							}
							else {
								Preferences.debug("! ERROR: No b-value found....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
								outputTextArea.append("! ERROR: No b-value found....exiting algorithm \n");
								return false;
							}
						}
						else {
							Preferences.debug("! ERROR: No b-value found....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
							outputTextArea.append("! ERROR: No b-value found....exiting algorithm \n");
							return false;
						}
					}
				}
			}
		}
		
		//lets output the b-values to screen:
		Preferences.debug(" - [  ", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append(" - [  ");
		for (int i = 0; i < bValuesArrayList.size(); i++) {
			float b = ((Float)bValuesArrayList.get(i)).floatValue();
			Preferences.debug(String.valueOf(b) + "   ", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(String.valueOf(b) + "   ");
		}
		Preferences.debug("] \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append("] \n");
		
		//check that num of b-values and num vols in gradient file are same
		if(bValuesArrayList.size() != direction.length) {
			Preferences.debug("! ERROR: the num of b values obtained and the number of vols in the gradient file are not the same....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append("! ERROR: the num of b values obtained and the number of vols in the gradient file are not the same....exiting algorithm \n");
			return false;
		}
		
		return true;
	}
	
	
	
	/**
	 * This method creates the b-matrix file
	 * @return boolean success
	 */
	public boolean createBMatrixFile() {
		try {
			StringBuffer sb;
			int padLength;
			File bMatrixFile = new File(studyPath + "_proc" + File.separator + studyName + ".BMTXT");
			FileOutputStream outputStream = new FileOutputStream(bMatrixFile);
			PrintStream printStream = new PrintStream(outputStream);
			DecimalFormat decFormat = new DecimalFormat("%16f");
			//formula for bmtxt values is : 
			//bxx 2bxy 2bxz byy 2byz bzz
			//x, y, and z values come from the direction[][]
			for (int i = 0; i < bValuesArrayList.size(); i++) {
				float b = ((Float)bValuesArrayList.get(i)).floatValue();
				float x = direction[i][0];
				float y = direction[i][1];
				float z = direction[i][2];
				
				float _bxx = b*x*x;
				if(Math.abs(_bxx) == 0) {
					_bxx = Math.abs(_bxx);
				}
				float _2bxy = 2*b*x*y;
				if(Math.abs(_2bxy) == 0) {
					_2bxy = Math.abs(_2bxy);
				}
				float _2bxz = 2*b*x*z;
				if(Math.abs(_2bxz) == 0) {
					_2bxz = Math.abs(_2bxz);
				}
				float _byy = b*y*y;
				if(Math.abs(_byy) == 0) {
					_byy = Math.abs(_byy);
				}
				float _2byz = 2*b*y*z;
				if(Math.abs(_2byz) == 0) {
					_2byz = Math.abs(_2byz);
				}
				float _bzz = b*z*z;
				if(Math.abs(_bzz) == 0) {
					_bzz = Math.abs(_bzz);
				}
				
				
				
				
				//following is for 1.4 compliant
				//otherwise, it would be : printStream.printf("%16f", b*x*x);
				String _bxx_string = String.valueOf(_bxx);
				int _bxx_stringLength = _bxx_string.length();
				sb = new StringBuffer(16);
				padLength = 16 - _bxx_stringLength;
				for(int j=0;j<padLength;j++) {
					sb.insert(j, " ");
				}
				sb.insert(padLength, _bxx_string);
				printStream.print(sb.toString());
				
				
				String _2bxy_string = String.valueOf(_2bxy);
				int _2bxy_stringLength = _2bxy_string.length();
				sb = new StringBuffer(16);
				padLength = 16 - _2bxy_stringLength;
				for(int j=0;j<padLength;j++) {
					sb.insert(j, " ");
				}
				sb.insert(padLength, _2bxy_string);
				printStream.print(sb.toString());
				
				
				String _2bxz_string = String.valueOf(_2bxz);
				int _2bxz_stringLength = _2bxz_string.length();
				sb = new StringBuffer(16);
				padLength = 16 - _2bxz_stringLength;
				for(int j=0;j<padLength;j++) {
					sb.insert(j, " ");
				}
				sb.insert(padLength, _2bxz_string);
				printStream.print(sb.toString());
				
				
				String _byy_string = String.valueOf(_byy);
				int _byy_stringLength = _byy_string.length();
				sb = new StringBuffer(16);
				padLength = 16 - _byy_stringLength;
				for(int j=0;j<padLength;j++) {
					sb.insert(j, " ");
				}
				sb.insert(padLength, _byy_string);
				printStream.print(sb.toString());
				
				
				String _2byz_string = String.valueOf(_2byz);
				int _2byz_stringLength = _2byz_string.length();
				sb = new StringBuffer(16);
				padLength = 16 - _2byz_stringLength;
				for(int j=0;j<padLength;j++) {
					sb.insert(j, " ");
				}
				sb.insert(padLength, _2byz_string);
				printStream.print(sb.toString());
				
				
				String _bzz_string = String.valueOf(_bzz);
				int _bzz_stringLength = _bzz_string.length();
				sb = new StringBuffer(16);
				padLength = 16 - _bzz_stringLength;
				for(int j=0;j<padLength;j++) {
					sb.insert(j, " ");
				}
				sb.insert(padLength, _bzz_string);
				printStream.print(sb.toString());
				
				printStream.println();

			}	
			outputStream.close();
		}
		catch (Exception e) {
			Preferences.debug(e.toString() + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("! ERROR: Creation of b-matrix file failed....exiting algorithm \n", Preferences.DEBUG_ALGORITHM);
			outputTextArea.append(e.toString() + "\n");
			outputTextArea.append("! ERROR: Creation of b-matrix file failed....exiting algorithm \n");
			return false;
		}
		
		
		Preferences.debug(" - b-matrix file created : " + studyPath + "_proc" + File.separator + studyName + ".BMTXT" + " \n", Preferences.DEBUG_ALGORITHM);
		outputTextArea.append(" - b-matrix file created : " + studyPath + "_proc" + File.separator + studyName + ".BMTXT" + " \n");
		return true;
	}

	
	
	
	/**
	 * this method cleans up the proc dir if success is false
	 * and it sets the lists to null
	 */
	public void finalize() {
		// delete any of the files that were created if success is false
		if(success == false) {
			if(isProcDirCreated) {
				Preferences.debug("! deleting proc dir \n", Preferences.DEBUG_ALGORITHM);
				if(outputTextArea != null) {
					outputTextArea.append("! deleting proc dir \n");
				}
				File procDir = new File(studyPath + "_proc");
				File[] files = procDir.listFiles();
				if (files.length > 0) {
					for(int i=0;i<files.length;i++) {
						files[i].delete();
					}
				}
				procDir.delete();
			}
		}

		fileInfoVector = null;
		seriesFileInfoTreeSet = null;
		seriesFileInfoTreeMap = null;
		success = true;
	}

	// -------------------------------INNER CLASS------------------------------------------------------

	/**
	 * This inner class is used to sort
	 * the list by instance number
	 */
	private class InstanceNumberComparator implements Comparator {
		public int compare(Object oA, Object oB) {
			FileInfoDicom fidA = (FileInfoDicom) oA;
			FileInfoDicom fidB = (FileInfoDicom) oB;
			String instancNoA_String = ((String) fidA.getValue("0020,0013")).trim();
			if (instancNoA_String == null) {
				instancNoA_String = "";
			}
			String instancNoB_String = ((String) fidB.getValue("0020,0013")).trim();
			if (instancNoB_String == null) {
				instancNoB_String = "";
			}
			if ((!instancNoA_String.equals("")) && (!instancNoB_String.equals(""))) {
				int instanceNoA = new Integer(instancNoA_String).intValue();
				int instanceNoB = new Integer(instancNoB_String).intValue();
				if (instanceNoA < instanceNoB) {
					return -1;
				}
				if (instanceNoA > instanceNoB) {
					return 1;
				}
			}
			return 0;
		}
	}
}
