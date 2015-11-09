package gov.nih.mipav.view.renderer.J3D.model.file;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.Vector;

import javax.vecmath.Point3f;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

import gov.nih.mipav.view.renderer.J3D.model.structures.ModelTriangleMesh;
import gov.nih.mipav.model.file.*;



/**
 * This class facilitates reading and writing vtk xml
 * @author pandyan
 *
 */
public class FileSurfaceVTKXML_J3D extends FileSurfaceVTKXML {
	
    /**
     * constructor
     * @param fName
     * @param fDir
     */
    public FileSurfaceVTKXML_J3D(String fName, String fDir) {
        super(fName, fDir);
    }
	
    /**
     * Saves as vtk xml
     * @param fileName
     * @param kMesh
     * @return boolean
     * @throws IOException
     */
    public boolean writeXMLsurface(String fileName, ModelTriangleMesh mesh) throws IOException {
        FileWriter fw;
        File headerFile;
        StringBuffer buff,buff2;
        headerFile = new File(fileName);
        fw = new FileWriter(headerFile);
        bw = new BufferedWriter(fw);
        
        int pointCount = mesh.getVertexCount();
        int indexCount = mesh.getIndexCount();
        double[][] scalars = mesh.getVertexData();
        double[][] cells = mesh.getCellData();
        
        openTag("?xml version=\"1.0\"?",true);
        openTag("VTKFile type=\"PolyData\"",true);
        openTag("PolyData",true);
        openTag("Piece NumberOfPoints=\"" + pointCount + "\" NumberOfPolys=\"" + indexCount/3 + "\"",true);
        openTag("Points",true);
        Point3f p=new Point3f();
        buff = new StringBuffer();
        for(int i=0;i<pointCount;i++){
            mesh.getCoordinate(i,p);
            buff.append(String.format("%.5f %.5f %.5f ", p.x,p.y,p.z));
        }
        Vector<XMLAttributes> atVector = new Vector<XMLAttributes>();
        atVector.add(new XMLAttributes("NumberOfComponents", "3"));
        atVector.add(new XMLAttributes("format", "ascii"));
        closedTag("DataArray",buff.toString(),atVector);
        openTag("Points",false);
        openTag("Polys",true);
        buff = new StringBuffer();
        buff2 = new StringBuffer();
        for(int i=0;i<indexCount;i+=3){
            buff.append(mesh.getCoordinateIndex(i) + " " + mesh.getCoordinateIndex(i+1) + " " + mesh.getCoordinateIndex(i+2) + " ");
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
        if(scalars!=null&&scalars.length>0&&scalars[0].length>0){
            buff = new StringBuffer();
            openTag("PointData Scalars=\"my_scalars\"",true);
            for(int i=0;i<scalars.length;i++){
                for(int j=0;j<scalars[i].length;j++){
                    buff.append(scalars[i][j]+" ");
                } 
            }
            atVector = new Vector<XMLAttributes>();
            atVector.add(new XMLAttributes("type", "Float32"));
            atVector.add(new XMLAttributes("Name", "my_scalars"));
            atVector.add(new XMLAttributes("format", "ascii"));
            closedTag("DataArrray",buff.toString(),atVector);
            openTag("PointData",false);
        }
        if(cells!=null&&cells.length>0&&cells[0].length>0){
            buff = new StringBuffer();
            openTag("CellData Scalars=\"cell_scalars\"",true);
            for(int i=0;i<cells.length;i++){
                for(int j=0;j<cells[i].length;j++){
                    buff.append(cells[i][j]+" ");
                } 
            }
            atVector = new Vector<XMLAttributes>();
            atVector.add(new XMLAttributes("type", "Int32"));
            atVector.add(new XMLAttributes("Name", "cell_scalars"));
            atVector.add(new XMLAttributes("format", "ascii"));
            closedTag("DataArrray",buff.toString(),atVector);
            openTag("CellData",false);
        }
        openTag("Piece", false);
        openTag("PolyData",false);
        openTag("VTKFile",false);
        bw.close();
        return true;
    }
	
    /**
     * Parses vtk xml
     * @param absPath
     * @return ModelTriangleMesh
     */
    public ModelTriangleMesh readXMLSurface(String absPath) {
        ModelTriangleMesh kMesh;
        String xsdFileName = "vtkSurface.xsd";
        int vertexCount;
        int indexCount;
        String pointsText;
        String polysConnectText = "";
        String polysOffsetsText = "";;
        Point3f[] points;
        int[] indices;
	
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
            points=new Point3f[vertexCount];
            String[] strs = pointsText.split("\\s+");
            for(int i=0;i<strs.length;i+=3){
                try {
                    Point3f p=new Point3f();
                    p.x=Float.parseFloat(strs[i]);
                    p.y=Float.parseFloat(strs[i+1]);
                    p.z=Float.parseFloat(strs[i+2]);
                    points[i/3]=p;
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
            indices=new int[indexCount*3];
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
			
			
            kMesh=new ModelTriangleMesh(points,indices);
            kMesh.setName(fileName);
	
        }catch(Exception e) {
            System.err.println("Error Parsing xml");
            e.printStackTrace();
            return null;
        }
        return kMesh;
    }
}
