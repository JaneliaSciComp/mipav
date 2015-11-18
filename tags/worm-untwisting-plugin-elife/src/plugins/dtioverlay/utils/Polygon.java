package dtioverlay.utils;

public class Polygon {
    Triangle faces[];
public BndBox bndBox;
    // Create a new Polyhedron from a list of vertices and faces.
    // Note: faces are zero-indexed into the vertex array
    public Polygon(double [][]vertData, double [][]faceData) {
        bndBox = new BndBox();
        PT []verts = new PT[vertData.length];
        for(int i=0;i<verts.length;i++) {
            verts[i] = new PT(vertData[i][0],vertData[i][1],vertData[i][2]);
            bndBox.union(verts[i]);
        }
        faces = new Triangle[faceData.length];
        for(int i=0;i<faces.length;i++) {
            //if(faceData[i].length<3) {
            //      System.out.println("Polygon face length: "+i+" "+faceData[i].length);
            //}
            faces[i] = new Triangle(verts[(int)faceData[i][0]],verts[(int)faceData[i][1]],
                    verts[(int)faceData[i][2]]);
        }
    }

    public boolean intersect(PT a, PT b) {
        for(int i=0;i<faces.length;) {
            try {
            IntersectResult ir = faces[i].findIntersect(a,b);
            if(ir.fractionalDistance>=0 && ir.fractionalDistance<=1)
            return true;
            else
            return false;

            } catch(Exception e) {
            return false;
            }

            /*try {
                if(faces[i].intersect(a,b))
                    return true;
            } catch(Exception e) {
                return false;
            } */
        }
        return false;
    }

    public IntersectResult reportIntersect(PT a, PT b) {
        for(int i=0;i<faces.length;i++) {
            try {
            IntersectResult ir = faces[i].findIntersect(a,b);
            if(ir.fractionalDistance>=0 && ir.fractionalDistance<=1)
                return ir;
            //else
            //    return null;

            } catch(Exception e) {
              //  return null;
            }

            /*try {
                if(faces[i].intersect(a,b))
                    return true;
            } catch(Exception e) {
                return false;
            } */
        }
        return null;
    }
}

