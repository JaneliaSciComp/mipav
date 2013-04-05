package gov.nih.mipav.model.algorithms.registration.vabra;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

public class VabraConfiguration extends DefaultHandler {
	ArrayList<Integer> levels;

	ArrayList<Integer> resolutionSwitches;

	ArrayList<Integer> downSampleResolutions;

	public VabraConfiguration(File file) {
		levels = new ArrayList<Integer>();
		resolutionSwitches = new ArrayList<Integer>();
		downSampleResolutions = new ArrayList<Integer>();
		readXML(file);
		//System.out.println(getClass().getCanonicalName()+"\t"+"LEVELS:"+levels.toString());
		//System.out.println(getClass().getCanonicalName()+"\t"+"DOWN SAMPLE RESOLUTIONS:"+downSampleResolutions.toString());
		//System.out.println(getClass().getCanonicalName()+"\t"+"RESOLUTION SWITCHES:"+resolutionSwitches.toString());
	}
	public boolean writeXML(File file) {	
		PrintWriter out;
		try {
			out = new PrintWriter(new BufferedWriter(new FileWriter(file)));
			StringBuffer buff=new StringBuffer();
			buff.append("<VABRA>\n");
			buff.append("\t<Levels>\n");
			for(Integer level:levels){
				buff.append("\t\t<Level xpoints=\""+level+"\"></Level>\n");
			}
			buff.append("\t</Levels>\n");
			buff.append("\t<Resolutions>\n");
			for(Integer sample:downSampleResolutions){
				buff.append("\t\t<Resolution downSampleFactor=\""+sample+"\"></Resolution>\n");
			}
			buff.append("\t</Resolutions>\n");
			buff.append("\t<ResolutionSwitchPoints>\n");
			for(Integer switches:resolutionSwitches){
				buff.append("\t\t<SwitchPoint level=\""+switches+"\"></SwitchPoint>\n");
			}
			buff.append("\t</ResolutionSwitchPoints>\n");
			buff.append("</VABRA>\n");
			System.out.println(getClass().getCanonicalName()+"\t"+buff.toString());
			
			out.print(buff.toString());
			out.flush();
			out.close();
			return true;
		} catch (IOException e) {
			System.err.println(getClass().getCanonicalName()+e.getMessage());
			return false;
		}

	}
	public void readXML(File file) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					new FileInputStream(file)));
			XMLReader xr = XMLReaderFactory.createXMLReader();
			xr.setContentHandler(this);
			xr.parse(new InputSource(in));
			in.close();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	public void startElement(String uri, String name, String qName,
			Attributes atts) {
		if(name.equalsIgnoreCase("Resolution"))downSampleResolutions.add(Integer.parseInt(atts.getValue("downSampleFactor")));
		if(name.equalsIgnoreCase("Level"))levels.add(Integer.parseInt(atts.getValue("xpoints")));
		if(name.equalsIgnoreCase("SwitchPoint"))resolutionSwitches.add(Integer.parseInt(atts.getValue("level")));
	}

	public void endElement(String uri, String name, String qName) {

	}
	public ArrayList<Integer> getDownSampleResolutions() {
		return downSampleResolutions;
	}
	public void setDownSampleResolutions(ArrayList<Integer> downSampleResolutions) {
		this.downSampleResolutions = downSampleResolutions;
	}
	public ArrayList<Integer> getLevels() {
		return levels;
	}
	public void setLevels(ArrayList<Integer> levels) {
		this.levels = levels;
	}
	public ArrayList<Integer> getResolutionSwitches() {
		return resolutionSwitches;
	}
	public void setResolutionSwitches(ArrayList<Integer> resolutionSwitches) {
		this.resolutionSwitches = resolutionSwitches;
	}
}
