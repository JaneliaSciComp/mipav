

import java.io.*;
import java.util.*;


import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.apache.axiom.om.OMAbstractFactory;
import org.apache.axiom.om.OMAttribute;
import org.apache.axiom.om.OMElement;
import org.apache.axiom.om.OMFactory;
import org.apache.axiom.om.impl.builder.StAXOMBuilder;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;


public class PlugInFileParser {
	
	private OMFactory factory;
	
	public PlugInFileParser(){
		factory = OMAbstractFactory.getOMFactory();
	}
	
	public void exportTxt(String name, String type, String version, File file, List<String> cols,
			List<PlugInGenomicsEntry> rows){
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			String delim = "";
			if(file.getName().endsWith(".csv"))
				delim = ",";
			else
				delim = "\t";
			if(name.endsWith(version))
				writer.write(name.substring(0, name.lastIndexOf(version)).trim()+delim+version);
			else
				writer.write(name.trim()+delim+version);
			writer.newLine();
			for(String col : cols)
				writer.write(col.trim()+delim);
			writer.newLine();
			
			for(PlugInGenomicsEntry row : rows){
				StringBuffer line = new StringBuffer();
				boolean empty=true;
				System.out.println(cols.size()+" "+row.getRowSize());
				for(int i=0;i<cols.size();i++){
					String value = String.valueOf(row.rowData[i+1]).trim();
					if(value!=null&&!value.trim().equals("")){
						if(value.equals("null"))
							line.append(""+delim);
						else{
							line.append(value+delim);
							empty=false;
						}
					}					
				}
				if(!empty){
					writer.write(line.toString());
					writer.newLine();
				}
			}
			writer.flush();
			writer.close();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void exportXml(String name, String type, String version, File file, List<String> cols,
			List<PlugInGenomicsEntry> rows){
		OMElement rootNode = factory.createOMElement(new QName("data_submission"));
		OMElement dataset = factory.createOMElement(new QName("data_set"));
		OMElement folder = factory.createOMElement(new QName("folder"));
		dataset.addAttribute("type", type, null);
		rootNode.addChild(dataset);
		rootNode.addChild(folder);

//		int indx=1;
		for(PlugInGenomicsEntry row : rows){
			OMElement elm = factory.createOMElement(new QName("data_structure"));
			if(name.endsWith(version))
				elm.addAttribute("name", 
						name.substring(0, name.lastIndexOf(version)).trim(), null);
			else
				elm.addAttribute("name", name.trim(), null);
			elm.addAttribute("version",version.trim(), null);
//			System.out.println(cols.size()+" "+row.getRowSize());
			for(int i=0;i<cols.size();i++){
				String value = String.valueOf(row.getValueAt(i+1)).trim();
				String col = cols.get(i);
//				System.out.println("n="+col+ " v="+value+" at "+i);
				OMElement ome = factory.createOMElement(new QName("data_element"));
				ome.addAttribute("name",(col.trim() != null?col.trim():""),null);
				ome.addAttribute("value",(value!= null&&!value.equals("null")?value:""),null);
				elm.addChild(ome);
			}
			rootNode.addChild(elm);
		}
		write(file, rootNode);
	}
	
	public void write(File file, OMElement root){
		Document document;
		try {
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            InputSource is = new InputSource(new StringReader(root.toString()));
			document = db.parse(is);
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			OutputFormat format = new OutputFormat(document);
			format.setLineWidth(65);
			format.setIndenting(true);
			format.setIndent(2);
	        XMLSerializer serializer = new XMLSerializer(writer, format);
	        serializer.serialize(document);	
			writer.flush();
			writer.close();
		} catch (SAXException e) {
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	@SuppressWarnings("unchecked")
	public List<PlugInGenomicsEntry> parseXml(File file){
		XMLStreamReader parser;
		try {
			parser = XMLInputFactory.newInstance().createXMLStreamReader(new FileInputStream(file));
			StAXOMBuilder builder = new StAXOMBuilder(parser);
			OMElement rootNode = builder.getDocumentElement();
			List<String> dataStructNames = new ArrayList<String>();
			//List<PlugInGenomicsEntry> entries = new ArrayList<PlugInGenomicsEntry>();
			//Document document = loadDocument(file, false, false);
			Iterator<OMElement> i = rootNode.getChildren();
			while(i.hasNext()){
				OMElement e = (OMElement) i.next();
				OMAttribute type = e.getAttribute(new QName("type"));
				System.out.println(type.getAttributeValue());
				Iterator<OMElement> j = e.getChildren();
				while(j.hasNext()){
					OMElement folder = (OMElement) j.next();
					Iterator<OMElement> k = folder.getChildren();
					while(k.hasNext()){
						OMElement dataStruct = (OMElement) k.next();
						dataStructNames.add(e.getAttribute(new QName("type")).getAttributeValue());
						Iterator<OMElement> l = dataStruct.getChildren();
						while(l.hasNext()){
							OMElement dataElement = (OMElement) l.next();
							String col = dataElement.getAttributeValue(new QName("name"));
							String value = dataElement.getAttributeValue(new QName("value"));
							System.out.println("Col: "+col);
							System.out.println("Value: "+value);
							if(!l.hasNext()){
//								GenomicsEntry row = new GenomicsEntry(new Object[col]);
//								row.setValueAt(value, col);
//								entries.add(row);
							}
						}
					}
				}
				System.out.println(i.next());
				return null;
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (XMLStreamException e) {
			e.printStackTrace();
		} catch (FactoryConfigurationError e) {
			e.printStackTrace();
		} //catch (IOException e) {
			//e.printStackTrace();
		//} catch (SAXException e) {
			//e.printStackTrace();
		//} catch (ParserConfigurationException e) {
			//e.printStackTrace();
		//}
		return null;
	}
	
	public Document loadDocument(File file,
			boolean setValidating, boolean setNamespaceAware)
			throws IOException, SAXException, ParserConfigurationException
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(setValidating);
		factory.setNamespaceAware(setNamespaceAware);

		DocumentBuilder builder = factory.newDocumentBuilder();
		return builder.parse(file);
	}
	
	public List<String> parseDataStructInfo(File file){
		BufferedReader reader;
		try {
			reader = new BufferedReader(new FileReader(file));
			List<String> info = new ArrayList<String>();
			String delim = "";
			if(file.getName().endsWith(".csv"))
				delim = ",";
			else
				delim = "\t";
			
			String[] in = reader.readLine().split(delim);
			System.out.println(in);
			for(int i=0;i<in.length;i++)
				info.add(in[i]);
			
			return info;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch(NullPointerException e){
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public List<String>  parseColumns(File file){
		BufferedReader reader;
		try {
			reader = new BufferedReader(new FileReader(file));
			List<String> columns = new ArrayList<String>();
			String delim = "";
			if(file.getName().endsWith(".csv"))
				delim = ",";
			else
				delim = "\t";
			//skip to second line
			reader.readLine();
			String[] cols = reader.readLine().split(delim);			
			for(int i=0;i<cols.length;i++){
				columns.add(cols[i]);
			}
			
			return columns;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch(NullPointerException e){
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	public List<PlugInGenomicsEntry> parseValues(File file) {
		try {
			BufferedReader reader = new BufferedReader(new FileReader(file));
			String line="";
			String delim = "";
			if(file.getName().endsWith(".csv"))
				delim = ",";
			else
				delim = "\t";
			int i=0;
			List<PlugInGenomicsEntry> entries = new ArrayList<PlugInGenomicsEntry>();
			while((line = reader.readLine()) != null){
				PlugInGenomicsEntry entry;
				if(i==0 || i==1){
				}
				//data lines
				else{
					entry = new PlugInGenomicsEntry(line.split(delim));
					entries.add(entry);
				}
				i++;
			}
			return entries;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
}
