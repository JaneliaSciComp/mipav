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
	
	private File keyFile;
	
	private FileWriter outputCSV;
	
	private ArrayList<File> dataFiles;
	
	private int keySIDind;
	
	private int keyGUIDind;
	
	private int dataSIDind;
	
	private int dataGUIDind;
	
	private boolean demoGUID;
	
	//NOTE: Depending on format, it may not work to use hashtable
	private Hashtable<String, String> keyTable;
	
	public PlugInAlgorithmKeyChecker(File key, ArrayList<File> data, boolean demo){
		keyFile = key;
		dataFiles = data;
		keyTable = new Hashtable<String, String>();
		demoGUID = demo;
		
		if(demoGUID)
			keyGUIDind = 1;
		else keyGUIDind = 2;
		keySIDind = 0;
		
		dataSIDind = 2;
		dataGUIDind = 1;
		
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
	
	private void compareDataFiles() throws IOException{
		
		for(File f : dataFiles){
			ArrayList<String> outList = new ArrayList<String>();
			outputCSV.append(f.getName() + "\r\n");
			Scanner scan = new Scanner(f);
			Scanner dataReader = scan.useDelimiter("\\r\\n");
			String line;
			
			//Remove header lines from file
			dataReader.next();
			
			String headerStr = dataReader.next();
			String[] header = headerStr.split(","); //Assumed CSV, change if not
			for(int i=0;i<header.length;i++){
				String cell = header[i];
				if(cell.contains("SubjectID"))
					dataSIDind = i;
				else if(cell.contains("GUID"))
					dataGUIDind = i;
			}
			//System.out.println(f.getName());
			
			while(dataReader.hasNext()){
				line = dataReader.next();
				if(line.startsWith(","))
					continue;
				line = line.replace("\"", "");
				String[] split = line.split(",");
				if(split.length < 3){
					continue;
				}
				
				//System.out.println(line);
				String SID = split[dataSIDind];
				String GUID = split[dataGUIDind];
				
				String keyGUID = keyTable.get(SID);
				
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
	
	private void readKeyFile() throws IOException{
		Scanner scan = new Scanner(keyFile);
		Scanner keyReader = scan.useDelimiter("\\r\\n");
		String line;
		
		/*String headerStr = keyReader.readLine();
		String[] header = headerStr.split(","); //Assumed CSV, change if not
		for(int i=0;i<header.length;i++){
			String cell = header[i];
			if(cell.contains("Subject ID"))
				keySIDind = i;
			else if(cell.contains("GUID"))
				keyGUIDind = i;
		}*/
		
		//Remove header lines from file
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
			
			if(!GUIDset.add(GUID)){
				outputCSV.append(GUID + "is duplicated in key file \r\n");
			}
			
			keyTable.put(SID, GUID);
		}
		
		scan.close();
		keyReader.close();
		
	}

}
