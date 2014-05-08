import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;

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
import java.util.Vector;


public class PlugInAlgorithmCompleteCatalog extends AlgorithmBase {

    private final File catalogFile;

    private final FileWriter csv;

    private Vector<String> catalogID;

    private Vector<String> visit;

    private Vector<Integer> type;

    private int[] complete;

    //private final int remove;
    
    private int GUID = -1;
    
    private int site = -1;
    
    private int visitID = -1;
    
    private int specimen = -1;

    public PlugInAlgorithmCompleteCatalog(final File file, final FileWriter out/*,final int lines*/) {
        super();
        catalogFile = file;
        csv = out;
        //remove = lines;
    }

    @Override
    public void runAlgorithm() {
        // TODO Auto-generated method stub
        complete = new int[6]; // RNA, Plasma, Serum, CSF, Blood
        final String[] types = {"RNA", "Plasma", "Serum", "CSF", "Blood", "DNA"};
        String GUIDStr;
        Integer typeNum;

        readCSV();
        
        if(site == -1 || GUID == -1 || visitID == -1 || specimen == -1){
        	MipavUtil.displayError("A header is missing");
        	return;
        }
        
        while (catalogID.size() > 0) {
            GUIDStr = catalogID.get(0);
            typeNum = type.get(0);
            if (compare(GUIDStr, typeNum)) {
                complete[typeNum.intValue()]++;
            }
        }

        try {
            csv.append("Sample Type,Number of Complete Sets\n");
            for (int i = 0; i < 6; i++) {
                csv.append(types[i] + "," + String.valueOf(complete[i]) + "\n");
            }
            csv.flush();
            csv.close();
            
            uniqueSample();
        } catch (final IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        setCompleted(true);

    }

    private boolean compare(final String GUID, final Integer which) {

        // First element is not removed already from each vector

        int index;
        int max;
        int month;
        int num;
        String[] visitType;
        final Vector<String> IDvisit = new Vector<String>();
        IDvisit.add(visit.get(0));
        while (catalogID.contains(GUID)) {
            index = catalogID.indexOf(GUID);
            while ( !type.get(index).equals(which)) {
                index = catalogID.indexOf(GUID, index + 1);
                if (index == -1) {
                    break;
                }
            }
            if (index == -1) {
                break;
            }
            catalogID.remove(index);
            if ( !IDvisit.contains(visit.get(index))) {
                IDvisit.add(visit.get(index));
            }
            visit.remove(index);
            type.remove(index);

        }

        max = 0;
        if(IDvisit.size()<2) return false;
        for (int i = 0; i < IDvisit.size(); i++) {
            visitType = IDvisit.get(i).split(" ");
            if (visitType.length == 2) {
                month = Integer.parseInt(visitType[0]);
                if (month > max) {
                    max = month;
                }
            }
        }
        if(which.equals(new Integer(3)))
        	num = max / 12 + 1;
        else
        	num = max / 6 + 1;
        return (num == IDvisit.size());
    }

    private void readCSV() {

        String[] lineArray;
        String typeStr;
        int typeNum;
        try {
            final BufferedReader input = new BufferedReader(new FileReader(catalogFile));
            try {
                String line = null;
                catalogID = new Vector<String>();
                visit = new Vector<String>();
                type = new Vector<Integer>();

                /*for (int i = 0; i < remove; i++) {
                    input.readLine();
                }*/
                
                String header = input.readLine();
                String[] hArray = parseLine2(header);
                for(int i=0;i<hArray.length;i++){
                	String hStr = hArray[i].trim();
                	if(hStr.equals("Site"))
                		site = i;
                	else if(hStr.equals("GUID"))
                		GUID = i;
                	else if(hStr.equals("Visit ID"))
                		visitID = i;
                	else if(hStr.equals("Site Specimen Type"))
                		specimen = i;
                }

                while ( (line = input.readLine()) != null) {
                    lineArray = parseLine2(line);
                    if (lineArray.length != 0) {

                        typeStr = lineArray[specimen].trim().toLowerCase();

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
                            typeNum = -1;
                        }

                        if (typeNum != -1) {
                            type.add(new Integer(typeNum));
                            catalogID.add(lineArray[GUID].trim());
                            visit.add(lineArray[visitID].trim());
                        }
                    }
                }
            } finally {
                input.close();
            }
        } catch (final IOException ex) {
            ex.printStackTrace();
        }
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
    	FileWriter uniqueCSV = new FileWriter(outFile);
    	
    	String uniqueHeader = "Visit Type,Study,DNA,RNA,Plasma,Serum,CSF,WB\n";
    	uniqueCSV.append(uniqueHeader);
    	/*for(int i=0;i<remove;i++){
    		input.readLine();
    	}*/
    	input.readLine();
    	//System.out.printf("%d %d %d %d\n", GUID, site, visitID, specimen);
    	
    	String line = null;
    	while ( (line = input.readLine()) != null) {
    		String[] lineArray = parseLine2(line);
    		if(lineArray.length == 0)
    			continue;
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
            }
    		
    		String setStr = siteStr + ";" + GUIDStr + ";" + visitStr + ";" + types[typeNum];
    		
    		siteSet.add(siteStr);
    		visitSet.add(visitStr);
    		//specSet.add(types[typeNum]);
    		lineSet.add(setStr);
    	}
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
    	
    	for(int i=0;i<visitSize;i++){
    		for(int j=0;j<siteSize;j++){
    			String output = visitList.get(i) + "," + siteList.get(j);
    			for(int k=0;k<specSize;k++){
    				int index = k + j*siteSize + i*siteSize*visitSize;
    				output += "," + counter[index];
    			}
    			output += "\n";
    			uniqueCSV.write(output);
    		}
    	}
    	
    	/*for(int i=0;i<specSize;i++){
    		String spec = specSet.get(i);
    		for(int j=0;j<visitSize;j++){
    			String visit = visitList.get(j);
    			for(int k=0;k<siteSize;k++){
    				String site = siteList.get(k);
    				int index = k + j*siteSize + i*siteSize*visitSize;
    				String output = String.format("%s,%s,%s,%d\n", site, visit, spec, counter[index]);
    				uniqueCSV.write(output);
    				//Write output
    			}
    		}
    	}*/
    	
    	uniqueCSV.close();
    	input.close();
    	//ArrayList<String> lineList = new ArrayList<String>(lineSet);
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
	
}
