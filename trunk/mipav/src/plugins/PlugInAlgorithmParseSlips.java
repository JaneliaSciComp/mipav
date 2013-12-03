import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;



public class PlugInAlgorithmParseSlips extends AlgorithmBase{
	
	private Vector<String[]> reportLines;
	private Vector<String[]> slipLines;
	private Vector<String> reportID;
	private Vector<String> slipID;
	private File reportFile;
	private File slipFile;
	private FileWriter csv;
	
	public PlugInAlgorithmParseSlips(){
		super();
	}
	public PlugInAlgorithmParseSlips(File report, File slip, FileWriter csv){
		super();
		reportFile = report;
		slipFile = slip;
		this.csv = csv;
	}
	
	@Override
	public void runAlgorithm() {
		// TODO Auto-generated method stub
		int index;
		String slipIDval;
		String[] reportString;
		String[] slipString;
		readCSV();
		for(int i=0;i<slipLines.size();i++){
			slipIDval = slipID.remove(0);
			slipString = slipLines.remove(0);
			if(reportID.contains(slipIDval)){
				index = reportID.indexOf(slipIDval);
				reportID.remove(index);
				reportString = reportLines.remove(index);
				
				compare(reportString, slipString);
			} 
			else{
				try {
					csv.append(slipIDval.concat(",not in report\n"));
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
		}
		try {
			csv.flush();
			csv.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		setCompleted(true);
	}
	
	public void setCSV(FileWriter out){
		this.csv = out;
	}
	
	public void setReportFile(File report){
		this.reportFile = report;
	}
	
	public void setSlipFile(File slip){
		this.slipFile = slip;
	}
	
	private void compare(String[] reportString, String[] slipString){
		String base = new String(slipString[0] + ",," );
		String export = new String();
		//Subject ID check
		reportString[3] = reportString[3].replaceAll("\\s", "");
		slipString[1] = slipString[1].replaceAll("\\s", "");
		if(!reportString[3].equals(slipString[1])) export+=reportString[3] + "," + slipString[1] + ",";
		else export += ",,";
		//Gender check
		reportString[4] = reportString[4].replaceAll("\\s", "");
		slipString[2] = slipString[2].replaceAll("\\s", "");
		if(!reportString[4].equals(slipString[2])) export+=reportString[4] + "," + slipString[2] + ",";
		else export += ",,";
		//Alias ID check
		reportString[2] = reportString[2].replaceAll("\\s", "");
		slipString[3] = slipString[3].replaceAll("\\s", "");
		if(!reportString[2].equals(slipString[3])) export+=reportString[2] + "," + slipString[3] + ",";
		else export += ",,";
		//Side ID check
		if(! (reportString[0].equals(slipString[7]) || 
				reportString[0].contains(slipString[7]) 
				|| slipString[7].contains(reportString[0]))) export+=reportString[0] + "," + slipString[7] + ",";
		else export += ",,";
		//Volume check
		//if(!reportString[12].split(" ")[0].equals(slipString[8])) export=export.concat(",Volume");
		//Collection date check
		reportString[7] = reportString[7].replaceAll("\\s", "");
		slipString[10] = slipString[10].replaceAll("\\s", "");
		if(!reportString[7].equals(slipString[10])) export+=reportString[7] + "," + slipString[10] + ",";
		else export += ",,";
		//Recieved date
		//if(!reportString[14].equals(slipString[11])) export=export.concat(",Recieved Date");
		//Check if all the same
		if(! (export.split(",").length == 0)){
			try {
				export = base.concat(export);
				csv.append(export.concat("\n"));
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void readCSV(){

		String[] lineArray;
		try {
			BufferedReader input =  new BufferedReader(new FileReader(reportFile));
			try {
				String line = null; 
				reportLines = new Vector<String[]>();
				reportID = new Vector<String>();

				while (( line = input.readLine()) != null){
					//System.out.println(line);
					lineArray = line.split(",");
					if(lineArray.length != 0){
						reportLines.add(lineArray);
						reportID.add(lineArray[9]);
					}
				}
			}
			finally {
				input.close();
			}
			input =  new BufferedReader(new FileReader(slipFile));
			try {
				String line = null; 
				slipLines = new Vector<String[]>();
				slipID = new Vector<String>();
				while (( line = input.readLine()) != null){
					//System.out.println(line);
					lineArray = line.split(",");
					if(lineArray.length !=0){
						slipLines.add(lineArray);
						slipID.add(lineArray[0]);
					}
				}
			}
			finally {
				input.close();
			}
		}
		catch (IOException ex){
			ex.printStackTrace();
		}
	}

}
