package dtioverlay.utils;

import java.io.*;
import java.util.Vector;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Dec 2, 2005
 * Time: 1:00:14 PM
 * To change this template use Options | File Templates.
 */
public class DTIStudioReader {
    public static void main(String [] args)
    {
        /* Debug LE file reader
        try {
        LEFileReader r = new LEFileReader("c:\\test.le");
        for(float j=1;j<1e11;j+=100000) {
        float f = r.readFloat();
        if(j!=f) {
        System.out.println(j+" "+f);
        }
        }

        } catch (Exception e) {

        }
        */
        /*DTIStudioReader read = new DTIStudioReader();
        try {
            read.read("C:\\Documents and Settings\\bennett\\Desktop\\at1000\\Fiber-SCPL.dat");
        } catch (IOException  e) {
            System.out.println(e);
            e.printStackTrace();;
        }

        double faces[][] = new double[1][3];
        faces[0][0]=0;  faces[0][1]=1;  faces[0][2]=2;

        double pts[][] = new double[3][3];*/
        // X = []; Y = []; Z = [];
        /* xy-test
        pts[0][0] = 121; pts[0][1] = 139; pts[0][2] = 18;
        pts[1][0] = 123; pts[1][1] = 148; pts[1][2] = 18;
        pts[2][0] = 113; pts[2][1] = 149; pts[2][2] = 18;
        */
        /* // yz-test
        pts[0][0] = 128; pts[0][1] = 155; pts[0][2] = 65-25;
        pts[1][0] = 128; pts[1][1] = 149; pts[1][2] = 65-31;
        pts[2][0] = 128; pts[2][1] = 164; pts[2][2] = 65-32;
        */
        /*// xz-test
        pts[0][0] = 103; pts[0][1] = 155; pts[0][2] = 65-47;
        pts[1][0] = 113; pts[1][1] = 155; pts[1][2] = 65-44;
        pts[2][0] = 113; pts[2][1] = 155; pts[2][2] = 65-51;
        */
        /*long time = System.currentTimeMillis();
        double []intersect = read.findIntersectPolygon(pts,faces,1);
        double [][]res = read.getFiber(intersect[0]);
        for(int j=0;j<res.length;j++)
            System.out.println(res[j][0]+","+res[j][1]+","+res[j][2]+" = "+res[j][3]);
        long out = System.currentTimeMillis();
        System.out.println("Fibers Selected: "+intersect.length);
        System.out.println((out-time)/1000.f+ " for one triangle");*/
    	
    	DTIStudioReader reader = new DTIStudioReader();
    	String filename = "C:\\dti\\dtispot\\images\\Fiber.dat";
    	int[][][][] test;
    	//double r,g,b;
    	
    	
    	
    	try {
    		test = reader.readColorVolumeMIPAV(filename);
    		//System.out.println(test.length);
    		for(int i=0;i<test.length;i++) {
    			int[][][] test2 = test[i];
    			//System.out.println(" " + test2.length);
    			for(int k=0;k<test2.length;k++) {
    				int[][] test3 = test2[k];
    				//System.out.println("  " + test3.length);
    				for(int j=0;j<test3.length;j++) {
    					int[] test4 = test3[j];
    					if((test4[0] != 0) || (test4[1] != 0) || (test4[2] != 0)) {
    						System.out.println("x,y,z = " + i + ","  + k + "," + j);
    						System.out.println(" r = " + test4[0]);
    						System.out.println(" g = " + test4[1]);
    						System.out.println(" b = " + test4[2]);
    					}
    					//for(int m=0;m<test4.length;m++) {
    					//	if(test4[m] != 0) {
    							//System.out.println(test4[m]);
    					//		System.out.println("  " + test4.length);
    					//	}
    					//}test[130][166][3]

    				}
    			}
    		}
    		System.out.println("hello");
    		
    	}
    	catch(IOException e) {
    		System.out.println(e.toString());
    	}
    	
    }

    public double[] findIntersectPolygon(double[][] pts, double[][] faces, double marker) {
        Polygon p = new Polygon(pts,faces);
        Vector<Integer> v = new Vector<Integer>();
        for (int j=0;j<this.fibers.length;j++) {
            //fibers[j].intersects(p);
            IntersectResult r = fibers[j].reportiIntersect(p);
            if(r!=null) {
                v.add(new Integer(j));
                // need to track front/back with ab
                fibers[j].setMarkerPoint(marker,(int)r.fractionalDistance);
                fibers[j].setMarkerPoint(marker,(int)r.fractionalDistance-1);
                //System.out.println("hit");
            }
        }
        double res[] = new double[v.size()];
        Iterator<Integer> it = v.iterator();
        int i=0;
        while(it.hasNext()) {
            res[i] = (it.next()).doubleValue();
            i++;
        }
        return res;
    }

    public double[] findIntersectPolygonDTIStudioCoordinates(double[][] pts, double[][] faces, double marker) {
        // Switch from left-handed to right handed coordinate systems
        for(int i=0;i<pts.length;i++) {
                pts[i][2] = this.hdr.ImageSize[2]-pts[i][2];
            }
        return findIntersectPolygon(pts,faces,marker);
    }

    public double[][] getFiber(double index) {
        int idx = (int) index;
        if(idx<0 || idx>=fibers.length)
            return null;
        double [][]result = new double[fibers[idx].xyzChain.length][4];
        for(int j=0;j<fibers[idx].xyzChain.length;j++) {
            result[j][0]=fibers[idx].xyzChain[j].x;
            result[j][1]=fibers[idx].xyzChain[j].y;
            result[j][2]=fibers[idx].xyzChain[j].z;
            result[j][3]=fibers[idx].getMarker(j);
        }
        return result;
    }

    public double [][][]computeConnectivityContrast() {
        double [][][]result = new double[hdr.ImageSize[0]][hdr.ImageSize[1]][hdr.ImageSize[2]];
        for(int j=0;j<fibers.length;j++) {
            for(int k=0;k<fibers[j].xyzChain.length;k++) {
                try{
                result[Math.round(fibers[j].xyzChain[k].x)][Math.round(fibers[j].xyzChain[k].y)][Math.round(fibers[j].xyzChain[k].z)]++;
                } catch(Exception e) {
                    // ignroe possible out of bounds exceptions.
                }
            }
        }
        return result;
    }

    class DTIStudioHDR {
        int FiberNR;
        int FiberLenMax;
        float FiberLenMean;
        int ImageSize[];
        float VoxelSize[];
        String slice_ori;
        String slice_seq;
        int versionNum;
        public DTIStudioHDR() {
            ImageSize = new int[3];
            VoxelSize = new float[3];
        }

        public String readString(LEFileReader fp, int len) throws IOException {
            if(len>0) {
                char c =(char)fp.readByte();
                return c+readString(fp,len-1);
            }
            return "";
        }

        public void read(LEFileReader fp) throws IOException  {

            String FiberFileTag = readString(fp,8);
            if(FiberFileTag.compareTo("FiberDat")!=0)
                throw new IOException("Incompatible file type");


            FiberNR = fp.readInt();
            FiberLenMax = fp.readInt();
            FiberLenMean =fp.readFloat();
            ImageSize[0]= fp.readInt();
            ImageSize[1]= fp.readInt();
            ImageSize[2]= fp.readInt();
            VoxelSize[0]= fp.readFloat();
            VoxelSize[1]= fp.readFloat();
            VoxelSize[2]= fp.readFloat();
            int so= fp.readInt();
            switch(so){
                case 0:
                    slice_ori = "Coronal";
                    break;
                case 1:
                    slice_ori = "Axial";
                    break;
                case 2:
                    slice_ori = "Sagittal";
                    break;
                default:
                    slice_ori = "Unknown";
                    break;
            }

            so=fp.readInt();
            switch(so) {
                case 0: slice_seq = "Positive";
                    break;
                case 1: slice_seq = "Negative";
                    break;
                default: slice_seq = "Unknown";
                    break;
            }

            String version=readString(fp,8);
            if(0==version.compareTo("2005."))
                versionNum=2005;
            else
                versionNum=2000;

            fp.seek(128);
        }
    }

    public class Fiber{
        Map<Integer,Double> markers;
        BndBox bndBox;
        int length;
        byte status;
        RGB color;
        int selLenStart;
        int selLenEnd;
        XYZ xyzPtROI;
        XYZ xyzChain[];

        public void read(LEFileReader fp, int version) throws IOException {
            bndBox = new BndBox();
            length = fp.readInt();
            status = (byte)fp.readByte();
            color = RGB.read(fp);
            selLenStart = fp.readInt();
            selLenEnd = fp.readInt();
            if(version>=2005) {
                xyzPtROI = XYZ.read(fp);
            } else {
                xyzPtROI = null;
            }
            if(length>1000) {
                System.out.println(length);
            }
            xyzChain = new XYZ[length];
            for(int i=0;i<length;i++) {
                xyzChain[i] = XYZ.read(fp);
                bndBox.union(xyzChain[i]);
            }

        }
        public boolean intersects(Polygon p) {
            if(!bndBox.intersect(p.bndBox))
                return false;
            PT a = new PT(xyzChain[0].x,xyzChain[0].y,xyzChain[0].z);
            for(int i=1;i<xyzChain.length;i++) {
                PT b = new PT(xyzChain[i].x,xyzChain[i].y,xyzChain[i].z);
                if(p.intersect(a,b)) {
                    return true;
                }
                a = b;
            }
            return false;
        }
        public IntersectResult reportiIntersect(Polygon p) {
            //if(xyzChain[0].y>160)
            //        System.out.println("ouch");
            if(!bndBox.intersect(p.bndBox))
                return null;
            PT a = new PT(xyzChain[0].x,xyzChain[0].y,xyzChain[0].z);
            for(int i=1;i<xyzChain.length;i++) {
                PT b = new PT(xyzChain[i].x,xyzChain[i].y,xyzChain[i].z);
                IntersectResult r = p.reportIntersect(a,b);

                if(r!=null) {
                    r.fractionalDistance = i;  //record which fiber segment hit the polygon
                    return r;
                }
                a = b;
            }
            return null;
        }
        public void setMarkerPoint(double marker, int pt) {
            if(markers==null) {
                markers = new HashMap<Integer,Double>();
            }
            markers.put(new Integer(pt),new Double(marker));
        }

        public double getMarker(int pt) {
            if(markers==null)
                return 0;
            Double m = (Double)markers.get(new Integer(pt));
            if(m==null)
                return 0;
            return m.intValue();
        }
    }
    DTIStudioHDR hdr;
    Fiber fibers[];
    public double read(String filename) throws IOException {
        //RandomAccessFile fp = new RandomAccessFile(filename,"r");
        LEFileReader fp = new LEFileReader(filename);
        hdr = new DTIStudioHDR();
        hdr.read(fp);

        fibers = new Fiber[hdr.FiberNR];
        long time = System.currentTimeMillis();
        for(int j=0;j<hdr.FiberNR;j++) {
            Fiber f = new Fiber();
            f.read(fp,hdr.versionNum);
            fibers[j]=f;
            /*if(0==(j%10000)) {
            System.out.println(j+" "+fp.offset);
            }    */
        }
        long out = System.currentTimeMillis();

        System.out.println("read: "+ (out-time)/1000.f+ " sec for "+hdr.FiberNR+" fibers");
        return 0f;
    }

    public double[][][] readCC(String filename) throws IOException {
        //RandomAccessFile fp = new RandomAccessFile(filename,"r");
        LEFileReader fp = new LEFileReader(filename);
        hdr = new DTIStudioHDR();
        hdr.read(fp);

        double [][][]result = new double[hdr.ImageSize[0]][hdr.ImageSize[1]][hdr.ImageSize[2]];

        long time = System.currentTimeMillis();
        for(int j=0;j<hdr.FiberNR;j++) {
            Fiber f = new Fiber();
            f.read(fp,hdr.versionNum);
            for(int k=0;k<f.xyzChain.length;k++) {
                try{
                result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)]++;
                } catch(Exception e) {
                    // ignroe possible out of bounds exceptions.
                }
            }
        }
        long out = System.currentTimeMillis();

        System.out.println("readCC: "+(out-time)/1000.f+ " sec for "+hdr.FiberNR+" fibers");
        return result;
    }

        public double[][][][] readColorVolume(String filename) throws IOException {
        //RandomAccessFile fp = new RandomAccessFile(filename,"r");
        LEFileReader fp = new LEFileReader(filename);
        hdr = new DTIStudioHDR();
        hdr.read(fp);

        double [][][][]result = new double[hdr.ImageSize[0]][hdr.ImageSize[1]][hdr.ImageSize[2]][3];

        long time = System.currentTimeMillis();
        for(int j=0;j<hdr.FiberNR;j++) {
            Fiber f = new Fiber();
            f.read(fp,hdr.versionNum);
            for(int k=0;k<f.xyzChain.length;k++) {
                try{
                result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][0]=
                        (double) (f.color.r & 0xFF);
                result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][1]=
                        (double)(f.color.g & 0xFF);
                result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][2]=
                        (double)(f.color.b & 0xFF);
                } catch(Exception e) {
                    // ignroe possible out of bounds exceptions.
                }
            }
        }
        long out = System.currentTimeMillis();

        System.out.println("readColorVolume: " + (out-time)/1000.f+ " sec for "+hdr.FiberNR+" fibers");
        return result;
    }
        
        public int[][][][] readColorVolumeMIPAV(String filename) throws IOException {
            LEFileReader fp = new LEFileReader(filename);
            hdr = new DTIStudioHDR();
            hdr.read(fp);

            int [][][][]result = new int[hdr.ImageSize[0]][hdr.ImageSize[1]][hdr.ImageSize[2]][3];

            long time = System.currentTimeMillis();
            for(int j=0;j<hdr.FiberNR;j++) {
                Fiber f = new Fiber();
                f.read(fp,hdr.versionNum);
                for(int k=0;k<f.xyzChain.length;k++) {
                    try{
                    result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][0]=
                            (f.color.r & 0xff);
                    result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][1]=
                            (f.color.g & 0xff);
                    result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][2]=
                            (f.color.b & 0xff);
                    } catch(Exception e) {
                        // ignroe possible out of bounds exceptions.
                    }
                }
            }
            long out = System.currentTimeMillis();

            System.out.println("readColorVolumeMIPAV: " + (out-time)/1000.f+ " sec for "+hdr.FiberNR+" fibers");
            return result;
        }

    public double[][][][] readColorVolumeSoft(String filename) throws IOException {
    //RandomAccessFile fp = new RandomAccessFile(filename,"r");
    LEFileReader fp = new LEFileReader(filename);
    hdr = new DTIStudioHDR();
    hdr.read(fp);

    double [][][][]result = new double[hdr.ImageSize[0]][hdr.ImageSize[1]][hdr.ImageSize[2]][4];

    long time = System.currentTimeMillis();
        System.out.println(hdr.FiberNR);
    for(int j=0;j<hdr.FiberNR;j++) {
        Fiber f = new Fiber();
        f.read(fp,hdr.versionNum);
        for(int k=0;k<f.xyzChain.length;k++) {

            try{
            result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][0]+=
                    (double) (f.color.r & 0xFF);;
            result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][1]+=
                    (double) (f.color.g & 0xFF);
            result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][2]+=
                    (double) (f.color.b & 0xFF);;
            result[Math.round(f.xyzChain[k].x)][Math.round(f.xyzChain[k].y)][Math.round(f.xyzChain[k].z)][3]+=
                    1;
            } catch(Exception e) {
                // ignroe possible out of bounds exceptions.
              //  System.out.println("out of bounds");
            }
        }
    }
    long out = System.currentTimeMillis();

    System.out.println("readColorVolumeSoft: " +(out-time)/1000.f+ " sec for "+hdr.FiberNR+" fibers");
    return result;
}


}

