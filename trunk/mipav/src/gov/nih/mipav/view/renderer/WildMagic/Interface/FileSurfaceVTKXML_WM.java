package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.model.file.FileSurfaceVTKXML;
import gov.nih.mipav.model.structures.VOIContour;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Vector;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * This class facilitates reading and writing vtk xml
 * @author pandyan
 *
 */
public class FileSurfaceVTKXML_WM extends FileSurfaceVTKXML {
	
    /**
     * constructor
     * @param fName
     * @param fDir
     */
    public FileSurfaceVTKXML_WM(String fName, String fDir) {
        super(fName, fDir);
    }
	
    /**
     * Parses vtk xml
     * @param absPath
     * @return TriMesh
     */
    public TriMesh readXMLSurface_WM(String absPath)
    {
        TriMesh kMesh = null;
        String xsdFileName = "vtkSurface.xsd";
        int vertexCount;
        int indexCount;
        String pointsText;
        String polysConnectText = "";
        String polysOffsetsText = "";;
	
        try{
            //validate xml
            SchemaFactory factory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
            URL xsdURL = getClass().getClassLoader().getResource(xsdFileName);
            Schema schema = factory.newSchema(xsdURL);
            Validator validator = schema.newValidator();
            Source source = new StreamSource(absPath);
            validator.validate(source);
            System.out.println(absPath + " is valid");
			
			
            SAXBuilder builder = new SAXBuilder();
            Document doc= builder.build(new File(absPath));
            Element rootElement = doc.getRootElement();
            Element polyDataElement = rootElement.getChild("PolyData");
            Element pieceElement = polyDataElement.getChild("Piece");
            Attribute numPointsAttr = pieceElement.getAttribute("NumberOfPoints");
            vertexCount = Integer.parseInt(numPointsAttr.getValue());
            Attribute numPolysAttr = pieceElement.getAttribute("NumberOfPolys");
            indexCount = Integer.parseInt(numPolysAttr.getValue());
            Element pointsElement = pieceElement.getChild("Points");
            Element pointsDataArrayElement = pointsElement.getChild("DataArray");
            pointsText = pointsDataArrayElement.getText();
            Element polysElement = pieceElement.getChild("Polys");
            List polysDataArrayElementList = polysElement.getChildren("DataArray");
            Element polysDataArrayElement = (Element)polysDataArrayElementList.get(0);
            Attribute nameAttr = polysDataArrayElement.getAttribute("Name");
            String value = nameAttr.getValue();
            if(value.equals("connectivity")) {
                polysConnectText = polysDataArrayElement.getValue();
            } else if(value.equals("offsets")) {
                polysOffsetsText = polysDataArrayElement.getValue();;
            }
            polysDataArrayElement = (Element)polysDataArrayElementList.get(1);
            nameAttr = polysDataArrayElement.getAttribute("Name");
            value = nameAttr.getValue();
            if(value.equals("connectivity")) {
                polysConnectText = polysDataArrayElement.getValue();
            } else if(value.equals("offsets")) {
                polysOffsetsText = polysDataArrayElement.getValue();
            }
			
			
            //points
            Attributes kAttr = new Attributes();
            kAttr.SetPChannels(3);
            kAttr.SetNChannels(3);
            kAttr.SetTChannels(0,3);
            kAttr.SetCChannels(0,4);
            VertexBuffer kVBuffer = new VertexBuffer( kAttr, vertexCount );
            String[] strs = pointsText.split("\\s+");
            for(int i=0;i<strs.length;i+=3){
                try {
                    kVBuffer.SetPosition3( i/3,
                                           Float.parseFloat(strs[i]),
                                           Float.parseFloat(strs[i+1]),
                                           Float.parseFloat(strs[i+2]) );
                    kVBuffer.SetColor4(0, (i-1)/3, 1f, 1f, 1f, 1f );
                    //System.out.println(i/3+")"+p);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT VERTS");
                    return null;
                }
            }
			
            //offsets    (make sure that all the offsets are differences of 3...otherwise, a triangle mesh is notpossible)
            String[] strs2 = polysOffsetsText.split("\\s");
            System.out.println(strs2[0]);
            int offset;
            if(Integer.parseInt(strs2[0]) != 3) {
                System.err.println("Offsets are not differences of 3");
                return null;
            }
            offset = Integer.parseInt(strs2[0]);
            for(int i=1;i<strs2.length;i++) {
                int offset2 = Integer.parseInt(strs2[i]);
                if(offset2 - offset != 3) {
                    System.err.println("Offsets are not differences of 3");
                    return null;
                }
                offset = offset2;
            }
			
            //polygons
            int count=0;
            int[] indices=new int[indexCount*3];
            String[] strs3 = polysConnectText.split("\\s");
            for(int i=0;i<strs3.length;i+=3){			
                try {
                    indices[count++]=Integer.parseInt(strs3[i]);
                    indices[count++]=Integer.parseInt(strs3[i+1]);
                    indices[count++]=Integer.parseInt(strs3[i+2]);
                } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT INDICES");
                    return null;
                }
            }
			
            IndexBuffer kIBuffer = new IndexBuffer( indices.length, indices );
            kMesh=new TriMesh(kVBuffer, kIBuffer);
            kMesh.SetName(fileName);
	
        }catch(Exception e) {
            System.err.println("Error Parsing xml");
            e.printStackTrace();
            return null;
        }
        return kMesh;
    }
    
    /**
     * Output TriMesh as vtk xml
     * @param fileName file name
     * @param kMesh TriMesh
     * @return true on write success
     * @throws IOException I/O write error.
     */
    public boolean writeXMLsurface(String fileName, TriMesh kMesh) throws IOException {
        FileWriter fw;
        File headerFile;
        StringBuffer buff,buff2;
        headerFile = new File(fileName);
        fw = new FileWriter(headerFile);
        bw = new BufferedWriter(fw);
        
        int pointCount = kMesh.VBuffer.GetVertexQuantity();
        int indexCount = kMesh.IBuffer.GetIndexQuantity();
        
        openTag("?xml version=\"1.0\"?",true);
        openTag("VTKFile type=\"PolyData\"",true);
        openTag("PolyData",true);
        openTag("Piece NumberOfPoints=\"" + pointCount + "\" NumberOfPolys=\"" + indexCount/3 + "\"",true);
        openTag("Points",true);
        Vector3f p=new Vector3f();
        buff = new StringBuffer();
        for(int i=0;i<pointCount;i++){
            kMesh.VBuffer.GetPosition3(i,p);
            buff.append(String.format("%.5f %.5f %.5f ", p.X,p.Y,p.Z));
        }
        Vector<XMLAttributes> atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("NumberOfComponents", "3"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff.toString(),atVector);
        openTag("Points",false);
        
        
        openTag("Polys",true);
        buff = new StringBuffer();
        buff2 = new StringBuffer();
        int[] aiIndex = kMesh.IBuffer.GetData();
        for(int i=0;i<indexCount;i+=3){
            buff.append(aiIndex[i] + " " + aiIndex[i+1] + " " + aiIndex[i+2] + " ");
            int offset = i + 3;
            buff2.append(offset + " ");
        }
        atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("type", "Int32"));
        atVector.add(new XMLAttributes("Name", "connectivity"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff.toString(),atVector);
        atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("type", "Int32"));
        atVector.add(new XMLAttributes("Name", "offsets"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff2.toString(),atVector);
        openTag("Polys",false);
        
        
        openTag("Piece", false);
        openTag("PolyData",false);
        openTag("VTKFile",false);
        bw.close();
        return true;
    }

    

    public Vector<VOIContour> readXMLPolylines_WM(String absPath)
    {
        String xsdFileName = "vtkPolyline.xsd";
        int vertexCount;
        int lineCount;
        String pointsText;
        String linesConnectText = "";
        String linesOffsetsText = "";;

        Vector<VOIContour> polyLines = null;
        try{
            //validate xml
            SchemaFactory factory = SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
            URL xsdURL = getClass().getClassLoader().getResource(xsdFileName);
            Schema schema = factory.newSchema(xsdURL);
            Validator validator = schema.newValidator();
            Source source = new StreamSource(absPath);
            validator.validate(source);
            System.out.println(absPath + " is valid");
			
			
            SAXBuilder builder = new SAXBuilder();
            Document doc= builder.build(new File(absPath));
            Element rootElement = doc.getRootElement();
            Element polyDataElement = rootElement.getChild("PolyData");
            Element pieceElement = polyDataElement.getChild("Piece");
            Attribute numPointsAttr = pieceElement.getAttribute("NumberOfPoints");
            vertexCount = Integer.parseInt(numPointsAttr.getValue());
            Attribute numLinesAttr = pieceElement.getAttribute("NumberOfLines");
            lineCount = Integer.parseInt(numLinesAttr.getValue());
            Element pointsElement = pieceElement.getChild("Points");
            Element pointsDataArrayElement = pointsElement.getChild("DataArray");
            pointsText = pointsDataArrayElement.getText();
            Element linesElement = pieceElement.getChild("Lines");
            List linesDataArrayElementList = linesElement.getChildren("DataArray");
            Element linesDataArrayElement = (Element)linesDataArrayElementList.get(0);
            Attribute nameAttr = linesDataArrayElement.getAttribute("Name");
            String value = nameAttr.getValue();
            if(value.equals("connectivity")) {
                linesConnectText = linesDataArrayElement.getValue();
            } else if(value.equals("offsets")) {
                linesOffsetsText = linesDataArrayElement.getValue();;
            }
            linesDataArrayElement = (Element)linesDataArrayElementList.get(1);
            nameAttr = linesDataArrayElement.getAttribute("Name");
            value = nameAttr.getValue();
            if(value.equals("connectivity")) {
                linesConnectText = linesDataArrayElement.getValue();
            } else if(value.equals("offsets")) {
                linesOffsetsText = linesDataArrayElement.getValue();
            }
			
			
            //points
            Vector<Vector3f> pointList = new Vector<Vector3f>();
            String[] strs = pointsText.split("\\s+");
            for(int i=0;i<strs.length;i+=3){
                try {
                	pointList.add( new Vector3f (
                                           Float.parseFloat(strs[i]),
                                           Float.parseFloat(strs[i+1]),
                                           Float.parseFloat(strs[i+2]) ) );
                } catch(NumberFormatException e){
                    System.err.println("CANNOT FORMAT VERTS");
                    return null;
                }
            }
			
            
            //polylines:
            polyLines = new Vector<VOIContour>();
            String[] offSets = linesOffsetsText.split("\\s");
            String[] connectivity = linesConnectText.split("\\s");
            int connectivityIndex = 0;
            for ( int i = 0; i < lineCount; i++ )
            {
            	int lineLength = Integer.parseInt(offSets[i]);
            	VOIContour newPolyLine = new VOIContour(false);
            	for ( int j = 0; j < lineLength; j++ )
            	{
                	int pointIndex = Integer.parseInt(connectivity[connectivityIndex++]);
            		Vector3f point = pointList.elementAt(pointIndex);
            		newPolyLine.add(point);
            	}
            	polyLines.add(newPolyLine);
            }
	
        }catch(Exception e) {
            System.err.println("Error Parsing xml");
            e.printStackTrace();
            return null;
        }
        return polyLines;
    }
    
    /**
     * Output a collection of polylines as vtk xml
     * @param fileName file name
     * @param Vector<VOIContours>
     * @return true on write success
     * @throws IOException I/O write error.
     */
    public boolean writeXMLpolylines(String fileName, Vector<VOIContour> polylines) throws IOException {
        FileWriter fw;
        File headerFile;
        StringBuffer buff,buff2;
        headerFile = new File(fileName);
        fw = new FileWriter(headerFile);
        bw = new BufferedWriter(fw);
        
        int pointCount = 0;
        for ( int i = 0; i < polylines.size(); i++ )
        {
        	pointCount += polylines.elementAt(i).size();
        }
        int lineCount = polylines.size();
        
        openTag("?xml version=\"1.0\"?",true);
        openTag("VTKFile type=\"PolyData\"",true);
        openTag("PolyData",true);
        openTag("Piece NumberOfPoints=\"" + pointCount + "\" NumberOfLines=\"" + lineCount + "\"",true);
        
        openTag("Points",true);
        buff = new StringBuffer();
        buff2 = new StringBuffer();
        Vector3f point;
        for ( int i = 0; i < polylines.size(); i++ )
        {
        	for ( int j = 0; j < polylines.elementAt(i).size(); j++ )
        	{
        		point = polylines.elementAt(i).elementAt(j);
                buff.append(String.format("%.5f %.5f %.5f ", point.X,point.Y,point.Z));
        	}
        }
        Vector<XMLAttributes> atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("NumberOfComponents", "3"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff.toString(),atVector);
        openTag("Points",false);
        
        
        openTag("Lines",true);
        buff = new StringBuffer();
        buff2 = new StringBuffer();
        int pointCounter = 0;
        for ( int i = 0; i < polylines.size(); i++ )
        {
        	for ( int j = 0; j < polylines.elementAt(i).size(); j++ )
        	{
        		point = polylines.elementAt(i).elementAt(j);
                buff.append(pointCounter++ + " ");
        	}
        	buff2.append(polylines.elementAt(i).size() + " ");
        }
        
        atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("type", "Int32"));
        atVector.add(new XMLAttributes("Name", "connectivity"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff.toString(),atVector);
        atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("type", "Int32"));
        atVector.add(new XMLAttributes("Name", "offsets"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff2.toString(),atVector);
        openTag("Lines",false);
        
        openTag("Piece", false);
        openTag("PolyData",false);
        openTag("VTKFile",false);
        bw.close();
        return true;
    }

    
}
