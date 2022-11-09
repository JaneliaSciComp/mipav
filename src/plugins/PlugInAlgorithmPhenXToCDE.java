import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmPhenXToCDE extends AlgorithmBase {

	private File dataFile;
	
	private File idFile;
	
	public PlugInAlgorithmPhenXToCDE(File inFile, File protocolIDFile){
		dataFile = inFile;
		idFile = protocolIDFile;
	}
	
	@Override
	public void runAlgorithm() {
		
		Hashtable<String, String> idHash;
		try{
			idHash = readIDFile();
		}catch(IOException e){
			MipavUtil.displayError("Cannot read protocol file");
			e.printStackTrace();
			return;
		}
		ArrayList<ArrayList<String>> rows; 
		try{
			rows = readDataFile();
		}catch(IOException e){
			MipavUtil.displayError("Cannot read data file");
			e.printStackTrace();
			return;
		}
		
		String parent = dataFile.getParent();
		String name = dataFile.getName();
		String ext = ".csv";
		int extInd = name.lastIndexOf(".");
		if(extInd > -1){
			ext = name.substring(extInd);
			name = name.substring(0, extInd);
		}
		File outFile = new File(parent + File.separator + name + "_CDE" + ext);
		String contact = "https://www.phenxtoolkit.org/";
		
		String format = "%1$s,%2$s,Unique Data Element,,%3$s,%4$s,,%5$s,%6$s,%7$s," +
				"%8$s,%9$s,%10$s,%11$s,%12$s,%13$s,PhenX,,,," +
				",,,,,,,Adult;Pediatric" +
				",,,,,,,,,,,,,,,,,,Supplemental,Supplemental,Supplemental,Supplemental," +
				",,,,,,,,,,,,,,NIH CIT,,,PhenX,," + contact + "\r\n";
		//Name, Title, Desc, Type, Restriction, Min, Max, Values, Value Desc, units, comments, notes
		
		String header = "Variable Name,Title,Element Type,Definition,"
				+ "Short Description,Datatype,Maximum Character Quantity,"
				+ "Input Restriction,Minimum Value,Maximum Value,"
				+ "Permissible Values,Permissible Value Descriptions,"
				+ "Unit of Measure,Guidelines/Instructions,Notes,"
				+ "Preferred Question Text,Keywords,References,"
				+ "Historical Notes,See Also,Effective Date,Until Date,"
				+ "External ID.LOINC,External ID.SNOMED,External ID.caDSR,"
				+ "External ID.CDISC,External ID.NINDS,Population.All,"
				+ "Domain.General (For all diseases),Domain.Traumatic Brain Injury,"
				+ "Domain.Parkinson's disease,Domain.Friedreich's Ataxia,Domain.Stroke,"
				+ "Domain.Amyotrophic Lateral Sclerosis,Domain.Huntington's Disease,"
				+ "Domain.Multiple Sclerosis,Domain.Neuromuscular Diseases,"
				+ "Domain.Myasthenia Gravis,Domain.Spinal Muscular Atrophy,"
				+ "Domain.Duchenne Muscular Dystrophy/Becker Muscular Dystrophy,"
				+ "Domain.Congenital Muscular Dystrophy,Domain.Spinal Cord Injury,"
				+ "Domain.Headache,Domain.Epilepsy,Classification.General (For all diseases),"
				+ "Classification.Acute Hospitalized,Classification.Concussion/Mild TBI,"
				+ "Classification.Epidemiology,Classification.Moderate/Severe TBI: "
				+ "Rehabilitation,Classification.Parkinson's Disease,Classification."
				+ "Friedreich's Ataxia,Classification.Stroke,Classification.Amyotrophic "
				+ "Lateral Sclerosis,Classification.Huntington's Disease,Classification"
				+ ".Multiple Sclerosis,Classification.Neuromuscular Diseases,"
				+ "Classification.Myasthenia Gravis,Classification.Spinal Muscular "
				+ "atrophy,Classification.Duchenne Muscular Dystrophy/Becker Muscular "
				+ "Dystrophy,Classification.Congenital Muscular Dystrophy,Classification."
				+ "Spinal cord Injury,Classification.Headache,Classification.Epilepsy,"
				+ "Submitting Organization Name,Submitting Contact Name,Submitting "
				+ "Contact Information,Steward Organization Name,Steward Contact Name,"
				+ "Steward Contact Information\n";
		
		try{
			FileWriter fw = new FileWriter(outFile);
			fw.write(header);
			for(int i=0;i<rows.size();i++){
				ArrayList<String> row = rows.get(i);
				String var = row.get(0);
				int ind = var.indexOf("_");
				String varName = var.substring(ind);
				String varID = var.substring(0, ind);
				String protocol = idHash.get(varID.substring(2));
				varName = varName.replace("_", "");
				String desc = row.get(1);
				String docfile = row.get(2);
				String varType = row.get(3);
				String unit = row.get(4);
				String min = row.get(5);
				String max = row.get(6);
				String varTerm = row.get(11);
				String note = varID + ": " + protocol + ". " + "Variable term: " + varTerm + ".";
				if(docfile.length()>0)
					note += " Also see " + docfile;
				String comments = row.get(7);
				if(comments.length()>0)
					comments +=" ";
				comments += row.get(8);
				
				if(comments.contains(",")){
					comments = comments.replaceAll("\"", "");
					comments = "\"" + comments + "\"";
				}
				
				if(varName.length()>30){
					varName = varName.substring(0, 30);
				}
				
				String permValue = "";
				String permDesc = ""; 
				//System.out.println(var);
				
				for(int j=13;j<row.size();j++){
					String value = row.get(j);
					if(value.contains(",")){
						value = value.replaceAll("\"", "");
					}
					if(value.length() == 0)
						break;
					if(varType.equals("Enumerated") || varType.equals("String")){
						permValue += value + ";";
						permDesc += value + ";";
					}else if(varType.startsWith("Encoded")){
						//System.out.println(value);
						String[] split = value.split("=");
						String pValue = split[0].trim().replaceAll("[()]", "");
						String pDesc = "";
						if(split.length>1){
							pDesc= split[1].trim();
						}
						permValue += pValue + ";";
						permDesc += pDesc + ";";
					}else{
						System.out.println(var + ": " + varType + " has values;");
					}
				}
				if(permValue.length() > 0){
					permValue = permValue.substring(0, permValue.length()-1);
					permDesc = permDesc.substring(0, permDesc.length()-1);
				}
				
				if(permValue.contains(",")){
					permValue = "\"" + permValue + "\"";
				}
				if(permDesc.contains(",")){
					permDesc = "\"" + permDesc + "\"";
				}
				
				
				String dataType = "";
				String restriction = "";
				if(varType.equals("Enumerated")){
					dataType = "Alphanumeric";
					restriction = "Single Pre-Defined Value Selected";
				}else if(varType.equals("Integer") ||
						varType.equals("Numeric")){
					dataType = "Numeric";
					restriction = "Free-Form Entry";
				}else if(varType.startsWith("Encoded")){
					dataType = "Numeric";
					restriction = "Single Pre-Defined Value Selected";
				}else if(varType.equals("String")){
					dataType = "Alphanumeric";
					restriction = "Free-Form Entry";
				}else{
					System.out.println(var + ": variable type " + varType + " unknown");
				}
				
				String output = String.format(format, varName, desc, desc, dataType, restriction, min, max, permValue,
						permDesc, unit, comments, note, desc);
				fw.append(output);
				
				//Name, Title, Desc, Type, Restriction, Min, Max, Values, Value Desc, units, comments, notes, doc
			}
			
			fw.close();
			
			MipavUtil.displayInfo("Conversion finished. Output to " + outFile.getAbsolutePath());
		}catch(IOException e){
			MipavUtil.displayError("Cannot open output csv file. Check if it is already open");
			e.printStackTrace();
			return;
		}
		
		
		
	}
	
	private ArrayList<ArrayList<String>> readDataFile() throws IOException{
		ArrayList<ArrayList<String>> rows = new ArrayList<ArrayList<String>>();
		
		final BufferedReader input = new BufferedReader(new FileReader(dataFile));
		String line = null;
		input.readLine();//remove header
		
		while((line = input.readLine()) != null){
			ArrayList<String> cells = parseLine(line);
			if(cells.size()>0 && cells.get(0).length()>0){//If first cell is empty, it is one of those proprietary protocols
				rows.add(cells);
			}
		}
		
		input.close();
		
		return rows;
	}

	private Hashtable<String, String> readIDFile() throws IOException{
		Hashtable<String, String> idHash = new Hashtable<String, String>();
		
		final BufferedReader input = new BufferedReader(new FileReader(idFile));
		String line = null;
		input.readLine();//remove header
		
		while((line = input.readLine()) != null){
			ArrayList<String> cells = parseLine(line);
			if(cells.size()>0){
				String protocol = cells.get(1).trim();
				String id = cells.get(2).replace(" ", "");
				if(id.length()>6)
					id = id.substring(1);//Get rid of spurious lead character
				idHash.put(id, protocol);
			}
		}
		
		input.close();
		
		return idHash;
	}
	
	private ArrayList<String> parseLine(String line){
		ArrayList<String> cells = new ArrayList<String>();
		
		while(line.indexOf(",") > -1){
			int index = line.indexOf(",");
			if(line.startsWith("\"")){
				int endQuote = line.indexOf("\",");
				int other = line.indexOf("\"\",");//possible look-alike
				while(other+1 == endQuote){
					endQuote = line.indexOf("\",", endQuote+1);
					other = line.indexOf("\"\",", endQuote+1);
				}
				index = line.indexOf(",", endQuote);
			}
			String part = "";
			if(index > 0){
				part = line.substring(0, index);
			}
			cells.add(part.trim());
			line = line.substring(index+1);
		}
		
		//Still one left
		cells.add(line.trim());
		
		return cells;
	}
}
