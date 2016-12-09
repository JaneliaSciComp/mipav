import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Scanner;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmKeyChecker extends AlgorithmBase {
	
	/**
	 * Master file to compare other files to
	 */
	private File keyFile;
	
	private FileWriter outputCSV;
	
	/**
	 * List of files to compare to the master file
	 */
	private ArrayList<File> dataFiles;
	
	/**
	 * Indecies of the columns you want, as they sometimes
	 * change in between files. SID is Subject ID
	 */
	
	private int keySIDind;
	
	private int keyGUIDind;
	
	private int dataSIDind;
	
	private int dataGUIDind;
	
	private boolean demoGUID;
	
	/**
	 * Hash of Subject ID as the key and the associated GUID
	 * in the master file as the value. Used as the comparison
	 * data structure.
	 */
	private Hashtable<String, String> keyTable;
	
	public PlugInAlgorithmKeyChecker(File key, ArrayList<File> data, boolean demo){
		keyFile = key;
		dataFiles = data;
		keyTable = new Hashtable<String, String>();
		demoGUID = demo;
		
		//Initial guesses for where the columns are. For the
		//Master file, it shouldn't change.
		if(demoGUID)
			keyGUIDind = 1;
		else keyGUIDind = 2;
		keySIDind = 0;
		
		dataSIDind = 2;
		dataGUIDind = 1;
		
		//Build the output CSV file
		String keyDir = keyFile.getParent() + File.separator;
		String keyName = keyFile.getName();
		keyName = keyName.substring(0, keyName.lastIndexOf("."));
		keyName += "_keycompare.csv";
		String csvName = keyDir + keyName;

		try {
			outputCSV = new FileWriter(new File(csvName));
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}

	@Override
	public void runAlgorithm() {
		
		try{
			readKeyFile();
		} catch (IOException e){
			e.printStackTrace();
			MipavUtil.displayError("Unable to read Key file");
			return;
		} catch (Exception e){
			e.printStackTrace();
			MipavUtil.displayError("Error during Key file read");
			return;
		}
		
		try{
			compareDataFiles();
		} catch (IOException e){
			e.printStackTrace();
			MipavUtil.displayError("Unable to read Data file(s), or cannot save to CSV");
			return;
		} catch(Exception e){
			MipavUtil.displayError("Error during comparison");
			e.printStackTrace();
			return;
		}

		setCompleted(true);
		
	}
	
	/**
	 * Using the hash produced from the master file, go through each
	 * provided file and compare each Subject ID with the one in the
	 * hash table. Any differences should be noted in the output CSV.
	 * @throws IOException
	 */
	private void compareDataFiles() throws IOException{
		
		for(File f : dataFiles){
			ArrayList<String> outList = new ArrayList<String>();
			outputCSV.append(f.getName() + "\r\n");
			Scanner scan = new Scanner(f);
			Scanner dataReader = scan.useDelimiter("\\r\\n");
			String line;
			
			//Remove header lines from file
			dataReader.next();
			
			//The column each piece is in has changed, so need
			//to actively look at the column headers to determine
			//which columns we need to pull out
			String headerStr = dataReader.next();
			String[] header = headerStr.split(","); //Assumed CSV, change if not
			for(int i=0;i<header.length;i++){
				String cell = header[i];
				if(cell.contains("SubjectID"))
					dataSIDind = i;
				else if(cell.contains("GUID"))
					dataGUIDind = i;
			}
			
			
			while(dataReader.hasNext()){
				line = dataReader.next();
				if(line.startsWith(","))
					continue;
				line = line.replace("\"", "");
				String[] split = line.split(",");
				if(split.length < 3){
					continue;
				}
				
				String SID = split[dataSIDind];
				String GUID = split[dataGUIDind];
				
				String keyGUID = keyTable.get(SID);
				
				//Check to see if the GUID in the data file matches
				//with the GUID from the master hash
				//If the GUID is null (as is the case when weird 
				//formatting occurs), just skip this
				if(!GUID.equals(keyGUID) && keyGUID != null){
					//write to output
					outList.add(String.format(",%s,%s,%s%n", SID, keyGUID, GUID));
				}
			}
			if(outList.size() == 0){
				outputCSV.append(",All passed\r\n");
			} else {
				outputCSV.append(",Subject ID,Key GUID,Data GUID\r\n");
				for(String out : outList){
					outputCSV.append(out);
				}
			}
			scan.close();
			dataReader.close();
		}
		
		outputCSV.close();
		
	}
	
	/**
	 * Read through the master file to pull out the Subject ID
	 * and GUID pairings. It will also check to see if any GUIDs
	 * are duplicated for some reason. 
	 * @throws IOException
	 */
	private void readKeyFile() throws IOException{
		Scanner scan = new Scanner(keyFile);
		Scanner keyReader = scan.useDelimiter("\\r\\n");
		String line;
		
		/*String headerStr = keyReader.next();
		String[] header = headerStr.split(","); //Assumed CSV, change if not
		for(int i=0;i<header.length;i++){
			String cell = header[i];
			if(cell.contains("Subject ID"))
				keySIDind = i;
			else if(cell.contains("GUID"))
				keyGUIDind = i;
		}*/
		
		//Remove header lines from file
		//Assume position of Subject ID and GUID will not change
		//between different master files. The code commented above
		//can be used in case that is not the case.
		keyReader.next();
		keyReader.next();
		
		HashSet<String> GUIDset = new HashSet<String>();
		
		while(keyReader.hasNext()){
			line = keyReader.next();
			if(line.startsWith(",") || line.length() == 0)
				continue;
			String[] split = line.split(",");
			if(split.length < 3){
				continue;
			}
			String SID = split[keySIDind];
			String GUID = split[keyGUIDind];
			
			//Check GUID hash to determine if duplicate GUID occurs
			if(!GUIDset.add(GUID)){
				outputCSV.append(GUID + "is duplicated in key file \r\n");
			}
			
			//Add SID/GUID combo to hash. Do nothing if SID key already
			//exists, not sure if there needs to be anything done
			keyTable.put(SID, GUID);
		}
		
		scan.close();
		keyReader.close();
		
	}

}
