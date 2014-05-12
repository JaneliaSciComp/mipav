import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmUniqueSamplesCount extends AlgorithmBase {

	private File catalogFile;
	
	private int site = -1;
	
	private int GUID = -1;
	
	private int visitID = -1;
	
	private int specimen = -1;
	
	public PlugInAlgorithmUniqueSamplesCount(File inputFile){
		super();
		catalogFile = inputFile;
	}
	
	@Override
	public void runAlgorithm() {
		// TODO Auto-generated method stub
		try{
			uniqueSample();
		} catch(IOException e){
			e.printStackTrace();
		}

	}
	
	private String[] parseLine2(String line){
		String[] output = new String[20];
		int cnt = 0;
		int ind = 0;
		while(cnt<20){
			ind = line.indexOf(",");
			if(ind > 0)
				output[cnt] = line.substring(0, ind).trim();
			else if (ind == -1) break;
			else output[cnt] = "null";
			cnt++;
			line = line.substring(ind+1);
		}
		String[] realout = new String[cnt];
		for(int i=0;i<cnt;i++){
			realout[i] = output[i];
		}
		
		return realout;
	}
	
	private void uniqueSample() throws IOException{
    	
        LinkedHashSet<String> siteSet = new LinkedHashSet<String>();
        LinkedHashSet<String> visitSet = new LinkedHashSet<String>();
        //LinkedHashSet<String> specSet = new LinkedHashSet<String>();
        LinkedHashSet<String> lineSet = new LinkedHashSet<String>();
        ArrayList<String> specSet = new ArrayList<String>();
        specSet.add("DNA");
        specSet.add("RNA");
        specSet.add("Plasma");
        specSet.add("Serum");
        specSet.add("CSF");
        specSet.add("Blood");
        
        final String[] types = {"RNA", "Plasma", "Serum", "CSF", "Blood", "DNA", "Unknown"};
    	
    	final BufferedReader input = new BufferedReader(new FileReader(catalogFile));
    	String outFilename = catalogFile.getName();
    	outFilename = outFilename.substring(0, outFilename.lastIndexOf("."));
    	outFilename += "_unique.csv";
    	outFilename = catalogFile.getParent() + File.separator + outFilename;
    	File outFile = new File(outFilename);
    	
    	/*for(int i=0;i<remove;i++){
    		input.readLine();
    	}*/
    	String header = input.readLine();
        String[] hArray = parseLine2(header);
        for(int i=0;i<hArray.length;i++){
        	String hStr = hArray[i].trim();
        	if(hStr.equals("Site"))
        		site = i;
        	else if(hStr.equals("Site Tube ID"))
        		GUID = i;
        	else if(hStr.equals("Visit ID"))
        		visitID = i;
        	else if(hStr.equals("Site Specimen Type"))
        		specimen = i;
        }
        
        if(site == -1 || GUID == -1 || visitID == -1 || specimen == -1){
        	MipavUtil.displayError("A header is missing");
        	input.close();
        	return;
        }
        
        FileWriter uniqueCSV = new FileWriter(outFile);
    	
    	String uniqueHeader = "Visit Type,Study,DNA,RNA,Plasma,Serum,CSF,WB\n";
    	uniqueCSV.append(uniqueHeader);
    	//System.out.printf("%d %d %d %d\n", GUID, site, visitID, specimen);
    	
    	String line = null;
    	int num = 0;
    	while ( (line = input.readLine()) != null) {
    		String[] lineArray = parseLine2(line);
    		if(lineArray.length == 0)
    			continue;
    		num++;
    		String siteStr = lineArray[site].trim();
    		String GUIDStr = lineArray[GUID].trim();
    		String visitStr = lineArray[visitID].trim();
    		String typeStr = lineArray[specimen].trim().toLowerCase();
    		int typeNum;
    		//System.out.println(typeStr);
    		if (typeStr.contains("rna")) {
                typeNum = 0;
            } else if (typeStr.contains("plasma")) {
                typeNum = 1;
            } else if (typeStr.contains("serum")) {
                typeNum = 2;
            } else if (typeStr.contains("cereb")) {
                typeNum = 3;
            } else if (typeStr.contains("blood")) {
                typeNum = 4;
            } else if (typeStr.contains("dna")){
            	typeNum = 5;
            } else {
                typeNum = 6;
                System.err.println("Unknown");
            }
    		
    		String setStr = siteStr + ";" + GUIDStr + ";" + visitStr + ";" + types[typeNum];
    		
    		siteSet.add(siteStr);
    		visitSet.add(visitStr);
    		//specSet.add(types[typeNum]);
    		lineSet.add(setStr);
    	}
    	System.out.println("Set size: " + lineSet.size());
    	System.out.println("Lines read: " + num);
    	int siteSize = siteSet.size();
    	int visitSize = visitSet.size();
    	int specSize = specSet.size();
    	int combinations = siteSize*visitSize*specSize;
    	int[] counter = new int[combinations];
    	ArrayList<String> siteList = new ArrayList<String>(siteSet);
    	ArrayList<String> visitList = new ArrayList<String>(visitSet);
    	//ArrayList<String> specList = new ArrayList<String>(specSet);
    	
    	Comparator<String> comp = new Comparator<String>(){

			@Override
			public int compare(String o1, String o2) {
				if(o1.equals("Baseline"))
					return -1;
				else if(o2.equals("Baseline"))
					return 1;
				else{
					String[] s1 = o1.split(" ");
					String[] s2 = o2.split(" ");
					int i1 = Integer.valueOf(s1[0]);
					int i2 = Integer.valueOf(s2[0]);
					if(i1 < i2)
						return -1;
					else return 1;
				}
			}
    	};
    	
    	Collections.sort(siteList);
    	Collections.sort(visitList, comp);
    	
    	Iterator<String> iter = lineSet.iterator();
    	while(iter.hasNext()){
    		String lineStr = iter.next();
    		//System.out.println(lineStr);
    		String[] lineSplit = lineStr.split(";");
    		int siteInd = siteList.indexOf(lineSplit[0]);
    		int visitInd = visitList.indexOf(lineSplit[2]);
    		int specInd = specSet.indexOf(lineSplit[3]);
    		int comboInd = siteInd + visitInd*siteSize + specInd*siteSize*visitSize;
    		counter[comboInd]++;
    	}
    	
    	for(int i=0;i<siteSize;i++){
    		for(int j=0;j<visitSize;j++){
    			String output = visitList.get(j) + "," + siteList.get(i);
    			int total = 0;
    			for(int k=0;k<specSize;k++){
    				int index = i + j*siteSize + k*siteSize*visitSize;
    				total += counter[index];
    				output += "," + counter[index];
    			}
    			output += "\n";
    			if(total > 0)
    				uniqueCSV.write(output);
    		}
    	}
    	
    	uniqueCSV.close();
    	input.close();

    }

}
