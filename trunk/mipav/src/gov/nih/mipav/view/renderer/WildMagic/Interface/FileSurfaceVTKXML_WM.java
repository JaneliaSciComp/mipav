package gov.nih.mipav.view.renderer.WildMagic.Interface;

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

import WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.model.file.*;

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
}
