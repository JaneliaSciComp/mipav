package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import com.sun.opengl.util.*;
import java.nio.*;

public class VolumeRenderer {
    //note you can remove anything to do with NRRD:
  public static int USE_NRRD = 0; //take out nrrd references so we dont have to link libnrrd

 public static int  MAX_VOLREN_2DTEX = 512;
 public static int  MAX_VOLREN_3DTEX = 30;
 public static int  MAX_VOLREN_BRICKS = 128;
  private VectorMath math = new VectorMath();
 //========== NOTES ===========================================
// Standard brick vertex ordering:
//
//     (011)        (111)
//       6 +---------+ 7   Where 1's are the size of the brick
//        /|        /|      allong that axis
//       / |       / |
// (001)/  |(101) /  |
//   4 +---------+ 5 |
//     |   |     |   |(110) z axis
//     | 2 +-----+---+ 3    ^
//     |  /(010) |  /       |   y axis
//     | /       | /        | /
//     |/        |/         |/
//   0 +---------+ 1        +-------> x axis
//  (000)      (100)
//

//volren type, use this to specify the type of texturing to use
//right now only 3DExt is available
    public static int VolRenUnkown = 0;
    public static int VolRen2DTexture = 1;
    public static int VolRen3DTexture = 2;
    public static int VolRen3DExt = 3;

    private TLUT            tlut;       //texture lookup table
    private MetaVolume               m_vol;      //holds data
   //note: a volume is associated with a 3Dtexture (volume[i]<->texname3D[i])
    Volume[] volume = new Volume[MAX_VOLREN_3DTEX];//current volume(s)
    private int                     numSubVols; //how many sub volumes
    private byte[]          data;       //points to subvolume in TRexVolModel
    private int                     subVolNum;  //which sub volume are we? (not used)
    private int[] size = new int[3];    //origional volume(subVol)
    private float[] sizef = new float[3];   //relative size [0-1]
    private float[] origf = new float[3];   //origin of volume
    private float[] center = new float[3];  //center of the volume...
    private float[] spacing = new float[3]; //distance between samples(not used yet)
    private int type;       //how are we rendering this volume
                                       //see 'VolRenType' enum above

    private int[]  texname2D = new int[MAX_VOLREN_2DTEX]; //for 2D texture method

    private int[]  texname3D = new int[MAX_VOLREN_3DTEX];   //3D texture method

    private int m_bb;       //do we render the bounding box??
    private int m_bbb;      //do be render the bounding box brackets

    private GLU glu = new GLU();

    public VolumeRenderer(MetaVolume vm, int subVolNum)
    {
            //we dont need this crap?? not here anyway
            m_vol = vm;
            /*
            //data =    model->getData(subVolNum);
            size[0] = m_vol.volumes[subVolNum].xiSize;
            size[1] = m_vol.volumes[subVolNum].yiSize;
            size[2] = m_vol.volumes[subVolNum].ziSize;

            float maxsize = math.max(size[0], size[1]);
            maxsize = math.max(maxsize, size[2]);

            //sizef [0-1] ? usualy but could be arbitrary physical units (cm, in, ft)
            sizef[0] = m_vol.volumes[subVolNum].xfSize;// AFFINE(0, size[0], maxsize, 0, 1);
            sizef[1] = m_vol.volumes[subVolNum].yfSize;//AFFINE(0, size[1], maxsize, 0, 1);
            sizef[2] = m_vol.volumes[subVolNum].zfSize;//AFFINE(0, size[2], maxsize, 0, 1);

            center[0] = m_vol.volumes[subVolNum].xfPos;
            center[1] = m_vol.volumes[subVolNum].yfPos;
            center[2] = m_vol.volumes[subVolNum].zfPos;
          */
            for(int i=0; i<MAX_VOLREN_3DTEX; ++i){
                    texname3D[i] = -1; //indicates that texname3D has not been generated yet
                    volume[i] = null;
            }

            this.subVolNum = subVolNum;
            numSubVols = 0;

            tlut = null;
            //bounding boxes off
            m_bb	 = 0;
            m_bbb = 0;

            tlut = null;
      }

      //========================================================================
//------------------------------------------------------------------------
//========================================================================
      public int createVolume(GLAutoDrawable drawable, int type, Volume v)
      {
              GL gl = drawable.getGL();
              System.err.println("Creating one volume");
              this.type = type;
              volume[0] = new Volume();
              volume[0] = v;
              numSubVols = 1;

              if ( type == VolRen2DTexture ) {
                       System.err.println("2D texture mapping method is not implemented");
                      return 1;
              } else if ( type == VolRen3DTexture ) {
                      System.err.println("3D texture mapping method (OGL 1.2) is not implemented");
                      return 1;
              } else if ( type == VolRen3DExt ) {
                      if(texname3D[0] == -1){
                              gl.glGenTextures(1, texname3D, 0);
                              System.err.println("generating a texture = " + texname3D[0]);
                      }
                      return create3DtextureEXT(drawable, v.currentData, v, texname3D[0]);
              } else {
                      System.err.println("Error : VolumeRenderer::createVolume() : type unknown");
                      type = VolRenUnkown;
                      return 1;
              }
       }

       //-------------------------------------------------------------------
       public int createVolume(GLAutoDrawable drawable, int type, Volume[] v, int nVols)
       { //load tones of volumes in at once!!

               System.out.println("***Creating " + nVols + " volumes ");
               GL gl = drawable.getGL();
               this.type = type;
               int i;
               for(i=0; i<nVols; ++i){
                       volume[i] = new Volume();
                       volume[i] = v[i];
               }

               numSubVols = nVols;

               int err = 0;
               int sizez = 0;

               switch(type){
               case 1:  //  VolRen2DTexture:
                       System.err.println("2D texture mapping method is not implemented");
                       return 1;
               case 2: //  VolRen3DTexture:
                       System.err.println("3D texture mapping method (OGL 1.2) is not implemented");
                       return 1;
               case 3: // VolRen3DExt:

                       if(texname3D[0] == -1)
                               gl.glGenTextures(nVols, texname3D, 0);
                       for(i = 0; i<nVols; ++i){
                               err |= create3DtextureEXT(drawable, v[i].currentData, v[i], texname3D[i]);
                       }

                       return err;
               default:
                       System.err.println("Error : VolumeRenderer::createVolume() : type unknown");
                       type = VolRenUnkown;
                       return 1;
               }
         }

         //------------------------------------------------------------------------
         private int create3DtextureEXT(GLAutoDrawable drawable, byte[] uctex, Volume v, int texname)
         {
                 System.out.println("Creating 3D texture (EXT) " + v.xiSize + " " + v.yiSize + " " + v.ziSize);
                 GL gl = drawable.getGL();
                 int[] sz = new int[3];

                 sz[0] = v.xiSize;
                 sz[1] = v.yiSize;
                 sz[2] = v.ziSize;


                 gl.glEnable(GL.GL_TEXTURE_3D);

                 gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);

                 gl.glBindTexture(GL.GL_TEXTURE_3D, texname);
                 gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
                 gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);

                 gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_WRAP_R, GL.GL_CLAMP);

                 // gl.glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_CLAMP);

                 gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
                 gl.glTexParameteri(GL.GL_TEXTURE_3D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

                 ByteBuffer bBuffer = ByteBuffer.wrap(uctex);
                 bBuffer.rewind();
                 gl.glTexImage3D(GL.GL_TEXTURE_3D,
                         0,
                         GL.GL_INTENSITY8,
                         sz[0],
                         sz[1],
                         sz[2],
                         0,
                         GL.GL_LUMINANCE,
                         GL.GL_UNSIGNED_BYTE,
                         bBuffer );

                 //****** load color lookup table *****************************
                 if (tlut == null)
                         tlut = new TLUT();

                 System.out.println("3D texture (EXT) created =" + texname);
                 gl.glDisable(GL.GL_TEXTURE_3D);
                 return glVolErr(drawable, "create3Dtexture()");
        }

        //========================================================================
//------------------- Render some volumes --------------------------------
//========================================================================


//standard 3d view alligned sliceing
        public void renderVolume(GLAutoDrawable drawable, float sampleRate, double[] mv)
        {
               int i;
               GL gl = drawable.getGL();
                if ( type == VolRen2DTexture ) {
                        System.err.println("2D texture mapping method is not implemented");
                        return;
                } else if ( type == VolRen3DTexture ) {
                        System.err.println("3D texture mapping method (OGL 1.2) is not implemented");
                        return;
                } else if ( type == VolRen3DExt ) {

                        if(numSubVols > 1){
                                int[] order = new int[numSubVols];
                                float[] zval = new float[numSubVols];
                                for(i=0; i<numSubVols; ++i){ //initialize sorting crap
                                        order[i] = i;
                                        initVol(i);
                                        zval[i] = (float)(center[0]*mv[2] + center[1]*mv[6] + center[2]*mv[10]);
                                }
                                int tmp;
                                for(i=0; i< numSubVols-1; ++i){ //now sort
                                        for(int j=i+1; j<numSubVols; ++j){
                                                if(zval[order[i]] > zval[order[j]]){//test for swap
                                                        tmp = order[i];
                                                        order[i] = order[j];
                                                        order[j] = tmp;
                                                }
                                        }
                                }

                                for(i=0 ; i< numSubVols; ++i){ //finaly render
                                        initVol(order[i]);
                                        gl.glPushMatrix();
                                        gl.glTranslatef(center[0], center[1], center[2]);
                                        render3DVolumeEXTVA(drawable, sampleRate, mv, order[i]);
                                        gl.glPopMatrix();
                                }
                        }
                        else{
                                initVol(0);
                                gl.glTranslatef(center[0], center[1], center[2]);
                                mv[12] += center[0];
                                mv[13] += center[1];
                                mv[14] += center[2];
                                render3DVolumeEXTVA(drawable, sampleRate, mv, 0);
                        }
                        return;
                }
        }

        //------------ render a smaller volume ---------------------------------
// overloaded funciton gives access to subvolue-extents
        void renderVolume(GLAutoDrawable drawable, float sampleRate, // renders a smaller axis
                          double[] mv,    // mv[16], aligned cube with axis
                          float[] xext,	    // xext[2], extents from {0..1}
                          float[] yext,     // yext[2], extents from {0..1}
                          float[] zext)      // zext[2], extents from {0..1}
        {
                int i;
                GL gl = drawable.getGL();
                if ( type == VolRen2DTexture ) {
                        System.err.println("2D texture mapping method is not implemented");
                        return;
                } else if ( type == VolRen3DTexture ) {
                        System.err.println("3D texture mapping method (OGL 1.2) is not implemented");
                        return;
                } else if ( type == VolRen3DExt ) {

                        if(numSubVols > 1){
                                int[] order = new int[numSubVols];
                                float[] zval = new float[numSubVols];
                                for(i=0; i<numSubVols; ++i){ //initialize sorting crap
                                        order[i] = i;
                                        initVol(i);
                                        zval[i] = (float)(center[0]*mv[2] + center[1]*mv[6] + center[2]*mv[10]);
                                }
                                int tmp;
                                for(i=0; i< numSubVols-1; ++i){ //now sort
                                        for(int j=i+1; j<numSubVols; ++j){
                                                if(zval[order[i]] > zval[order[j]]){//test for swap
                                                        tmp = order[i];
                                                        order[i] = order[j];
                                                        order[j] = tmp;
                                                }
                                        }
                                }

                                for(i=0 ; i< numSubVols; ++i){ //finaly render
                                        initVol(order[i]);
                                        gl.glPushMatrix();
                                        //glTranslatef(center[0], center[1], center[2]);
                                        double[] modelMatrix = new double[16];
                                        gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, modelMatrix, 0);
                                        render3DVolumeEXTSV(drawable, sampleRate, modelMatrix, order[i], xext, yext, zext);
                                        gl.glPopMatrix();
                                }
                        }
                        else{

                                initVol(0);
                                //glTranslatef(center[0], center[1], center[2]);
                                double[] modelMatrix = new double[16];
                                gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, modelMatrix, 0);
                                render3DVolumeEXTSV(drawable, sampleRate, modelMatrix, 0, xext, yext, zext);
                        }
                        return;
                }
        }


        //------------------------------------------------------------------------
//-------------- View Aligned slices -------------------------------------
//------------------------------------------------------------------------

        private void render3DVolumeEXTVA(GLAutoDrawable drawable, float sampleFrequency,
                                         double[] mv, // mv[16]
                                         int v)
        {
                float vo[][] = { //vo[8][3], volume vertex edge coords
                        {0,	 0,	   0},
                        {sizef[0], 0,	   0},
                        {0,	 sizef[1], 0},
                        {sizef[0], sizef[1], 0},
                        {0,	 0,	   sizef[2]},
                        {sizef[0], 0,	   sizef[2]},
                        {0,	 sizef[1], sizef[2]},
                        {sizef[0], sizef[1], sizef[2]}};

                float tx[][] = { //tx[8][3], volume texture coords/ edge to edge (bad for bricks)
                                {0, 0, 0},
                                {1, 0, 0},
                                {0, 1, 0},
                                {1, 1, 0},
                                {0, 0, 1},
                                {1, 0, 1},
                                {0, 1, 1},
                                {1, 1, 1}};

                float axis[] = {0,0,1};   // axis[3]

                render3DVA(drawable, sampleFrequency, mv, v, vo, tx, axis);
          }

          //==========================================================================
//------------ render a small volume ---------------------------------------
//==========================================================================

          private void render3DVolumeEXTSV(GLAutoDrawable drawable, float sampleFrequency,
                                           double[] mv, //mv[16], model view matrix
                                           int v,	      //which volume to render
                                           float[] xext, //xext[2], extents along axies
                                           float[] yext,  // yext[2]
                                           float[] zext)  // zext[2]
          {
                  GL gl = drawable.getGL();
                  float[] ext= {origf[0]+sizef[0], origf[1]+sizef[1], origf[2]+sizef[2]};
                  float[] x = new float[2];
                  float[] y = new float[2];
                  float[] z = new float[2];
                  //cerr << "smalvol ext" << xext[0] << "-" << xext[1] << " " << yext[0] << "-" << yext[1]
                  //	 << " " << zext[0] <<  "-" << zext[1] << endl;
                  //get the new extents based on subvolumes size
                  x[0] = xext[0] > origf[0] ? (xext[0] < ext[0] ? xext[0] : ext[0]) : origf[0];
                  x[1] = xext[1] > origf[0] ? (xext[1] < ext[0] ? xext[1] : ext[0]) : origf[0];
                  y[0] = yext[0] > origf[1] ? (yext[0] < ext[1] ? yext[0] : ext[1]) : origf[1];
                  y[1] = yext[1] > origf[1] ? (yext[1] < ext[1] ? yext[1] : ext[1]) : origf[1];
                  z[0] = zext[0] > origf[2] ? (zext[0] < ext[2] ? zext[0] : ext[2]) : origf[2];
                  z[1] = zext[1] > origf[2] ? (zext[1] < ext[2] ? zext[1] : ext[2]) : origf[2];
                  //now see if we have something to render:
                  if((x[0] == x[1]) || (y[0] == y[1]) || (z[0] == z[1]))
                          return;

                  //cerr << "smalvol ext" << x[0] << "-" << x[1] << " " << y[0] << "-" << y[1]
                  //	 << " " << z[0] <<  "-" << z[1] << endl;
                  //now put the extents in the local subvolume space
                  x[0] -= origf[0];
                  x[1] -= origf[0];
                  y[0] -= origf[1];
                  x[1] -= origf[1];
                  z[0] -= origf[2];
                  z[1] -= origf[2];
                  /*
                  float vo[8][3] = { //volume vertex edge coords
              {x[0], y[0], z[0]},
              {x[1], y[0], z[0]},
              {x[0], y[1], z[0]},
              {x[1], y[1], z[0]},
              {x[0], y[0], z[1]},
              {x[1], y[0], z[1]},
              {x[0], y[1], z[1]},
              {x[1], y[1], z[1]}};
                  */
              float vo[][] = { //vo[8][3], volume vertex edge coords
                          {0, 0, 0},
                          {1, 0, 0},
                          {0, 1, 0},
                          {1, 1, 0},
                          {0, 0, 1},
                          {1, 0, 1},
                          {0, 1, 1},
                          {1, 1, 1}};

              float tx[][] = { //tx[8][3], volume vertex edge coords
                                  {x[0]/sizef[0], y[0]/sizef[1], z[0]/sizef[2]},
                                  {x[1]/sizef[0], y[0]/sizef[1], z[0]/sizef[2]},
                                  {x[0]/sizef[0], y[1]/sizef[1], z[0]/sizef[2]},
                                  {x[1]/sizef[0], y[1]/sizef[1], z[0]/sizef[2]},
                                  {x[0]/sizef[0], y[0]/sizef[1], z[1]/sizef[2]},
                                  {x[1]/sizef[0], y[0]/sizef[1], z[1]/sizef[2]},
                                  {x[0]/sizef[0], y[1]/sizef[1], z[1]/sizef[2]},
                                  {x[1]/sizef[0], y[1]/sizef[1], z[1]/sizef[2]}};

              float[] axis = {0,0,1};   // axis[3]

                                  //center it about {000}
                                  //glTranslatef(-(x[0]+(x[1]-x[0])/2),
                                  //		-(y[0]+(y[1]-y[0])/2),
                                  //		-(z[0]+(z[1]-z[0])/2));
              gl.glTranslatef(-.5f, -.5f, -.5f);
              render3DVA(drawable, sampleFrequency, mv, v, vo, tx, axis);

          }

          //------------------------------------------------------------------------
//------ Gineric render allong a vector ----------------------------------
//------------------------------------------------------------------------
          private void render3DVA(GLAutoDrawable drawable, float sampleFrequency,
                                                                          double[] mv,  // mv[16]
                                                                          int v,
                                                                          float[][] vo,	  //vo[8][3], volume vertex coords model-space coords
                                                                          float[][] tx,   //tx[8][3], texture vertex coords tex-space coords
                                                                          float[] axis)  //axis[3], axis to slice along world-space coords
          {
                  int i;
                  GL gl = drawable.getGL();
                  float[][] rv = new float[8][3];     //the rotated volume (may include a scale)
                  float maxval = -10; //(tmp)
                  float minval = 10;
                  int minvert = 0;
                  double[] mvinv = new double[16];

                  inverseMatrix(mvinv, mv); //invert model view matrix

                  for(i=0; i<8; ++i){   //translate model to world coords (view space)
                          translateV3(rv[i], mv, vo[i]);
                          if(maxval < Math.max(maxval, rv[i][2])){
                                  maxval = Math.max(maxval, rv[i][2]);
                          }
                          if(minval > Math.min(minval, rv[i][2])){
                                  minval = Math.min(minval, rv[i][2]);
                                  minvert = i;  //determine the starting point for slicing
                          }
                  }
                  // fix this function so that the eye is the universal reference point for all volumes!
                  //  we will get artifacts at brick boundaries otherwise!


                  //find the slice plane point 'sp' (initial) and the slice plane normal 'sn'
                  //sp is the sliceing starting point, simply the vertex farthest from the eye
                  float[] sp = {vo[minvert][0], vo[minvert][1], vo[minvert][2]};  // sp[3]
                  //float vpn[3] = {0,0,1};  //view plane normal, points to eye (temp variable)
                  float[] vpn = new float[3];
                  vpn[0] = axis[0]; vpn[1] = axis[1]; vpn[2] = axis[2];
                  float[] sn = new float[3];		    //slice plane normal
                  translateV3(sn, mvinv, vpn); //move vpn to sn (model space);
                  //note: sn & sp are defined in Model Space, ie the space where the volume
                  // is alligned with the (x,y,z) axies
                  float normsn = (float)Math.sqrt(sn[0]*sn[0] + sn[1]*sn[1] + sn[2]*sn[2]); //normalize
                  sn[0]/=normsn;
                  sn[1]/=normsn;
                  sn[2]/=normsn;

                  //now find the distance we need to slice (|max_vertex - min_vertex|)
                  float[] maxd = {0, 0, maxval}; //maxd[3], (tmp) only use z-coord (view space)
                  float[] mind = {0, 0, minval}; //mind[3], (tmp) ditto	    (view space)
                  float[] maxv = new float[2];
                  float[] minv = new float[3];	   //maxv[3], minv[3](tmp)
                  translateV3(maxv, mvinv, maxd); //translate back to model space
                  translateV3(minv, mvinv, mind); //ditto
                  maxv[0] -= minv[0]; //subtract
                  maxv[1] -= minv[1];
                  maxv[2] -= minv[2];

                  //now take the norm of this vector... we have the distance to be sampled
                  float dist = (float)Math.sqrt(maxv[0]*maxv[0] + maxv[1]*maxv[1] + maxv[2]*maxv[2]);

                  //draw a red bounding box
                  if(m_bbb == 1) renderBBoxBrackets(drawable, sizef[0], sizef[1], sizef[2]);
                  if(m_bb == 1)  renderBBox(drawable, sizef[0], sizef[1], sizef[2]);


                  gl.glDisable(GL.GL_LIGHTING); //light makes it look bad!

                  gl.glDisable(GL.GL_CULL_FACE);
                  gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
                  gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);

                  gl.glEnable(GL.GL_TEXTURE_3D);

                  System.err.println("drawing" + texname3D[v]);
                  gl.glBindTexture(GL.GL_TEXTURE_3D, texname3D[v]);

          // #ifdef WIN32
          // #if    WILDCAT
                  // gl.glEnable(GL.GL_TEXTURE_COLOR_TABLE_EXT);
          // #endif
          // #else
                  gl.glEnable(GL.GL_TEXTURE_COLOR_TABLE_SGI);
          // #endif

                  gl.glEnable(GL.GL_BLEND);
                  gl.glBlendFunc(GL.GL_ONE, GL.GL_ONE_MINUS_SRC_ALPHA);

                  gl.glColor4f(1f, 1f, 1f, 1f);
                  glVolErr(drawable, "drawVA");
                  //distance between samples
                  float dis = sizef[0] / (size[0] * sampleFrequency);
                  float[] del = {sn[0]*dis, sn[1]*dis, sn[2]*dis};  // del[3]

                  int samples = (int)((dist) / dis);//(total distance to be sampled)/(sample spacing)

                  //samples /= 40;
                  //del[0] *= 40;
                  //del[1] *= 40;
                  //del[2] *= 40;

                  float[][] poly = new float[6][3];   // for edge intersections
                  float[][] tcoord = new float[6][3]; // for texture intersections
                  float[][] tpoly = new float[6][3];  // for transformed edge intersections
                  int edges;	       // total number of edge intersections

                  //highly un-optimized!!!!!!!!!
                  for(i = 0 ; i < samples; ++i){ //for each slice
                          //increment the slice plane point by the slice distance
                          sp[0] += del[0];
                          sp[1] += del[1];
                          sp[2] += del[2];

                          edges = 0;
                          //now check each edge of the volume for intersection with..
                          //the plane defined by sp & sn
                          //front bottom edge
                          edges += intersect(vo[0], vo[1], tx[0], tx[1], rv[0], rv[1], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //front left edge
                          edges += intersect(vo[0], vo[2], tx[0], tx[2], rv[0], rv[2], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //front right edge
                          edges += intersect(vo[1], vo[3], tx[1], tx[3], rv[1], rv[3], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //left bottom edge
                          edges += intersect(vo[4], vo[0], tx[4], tx[0], rv[4], rv[0], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //right bottom edge
                          edges += intersect(vo[1], vo[5], tx[1], tx[5], rv[1], rv[5], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //front top edge
                          edges += intersect(vo[2], vo[3], tx[2], tx[3], rv[2], rv[3], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //back bottom edge
                          edges += intersect(vo[4], vo[5], tx[4], tx[5], rv[4], rv[5], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //back left edge
                          edges += intersect(vo[4], vo[6], tx[4], tx[6], rv[4], rv[6], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //back right edge
                          edges += intersect(vo[5], vo[7], tx[5], tx[7], rv[5], rv[7], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //back top edge
                          edges += intersect(vo[6], vo[7], tx[6], tx[7], rv[6], rv[7], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //left top edge
                          edges += intersect(vo[2], vo[6], tx[2], tx[6], rv[2], rv[6], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);
                          //right top edge
                          edges += intersect(vo[3], vo[7], tx[3], tx[7], rv[3], rv[7], sp, sn,
                                  poly[edges], tcoord[edges], tpoly[edges]);

                          if(edges == 3){ //no sort required for triangles
                                  //cerr << "doing a triangle" <<endl;
                                  gl.glBegin(GL.GL_TRIANGLES);
                                  {
                                          gl.glTexCoord3fv(tcoord[0], 0);
                                          gl.glVertex3fv(poly[0], 0);
                                          gl.glTexCoord3fv(tcoord[1], 0);
                                          gl.glVertex3fv(poly[1], 0);
                                          gl.glTexCoord3fv(tcoord[2], 0);
                                          gl.glVertex3fv(poly[2], 0);
                                  }
                                  gl.glEnd();
                          }
                          else { //compute convex hull and sort, a little piece of magic from:
                                  // B.M.E. Moret & H.D. Shapiro "P to NP" pp. 453

                                  float dx, dy, tt ,theta;  //tt= TempTheta
                                  float[] cen = new float[2];
                                  cen[0] = cen[1] = 0.0f;
                                  int next;
                                  int j;
                                  //rather than swap 3 arrays, only one?
                                  int[] order ={0,1,2,3,4,5};  // order[6]

                                  // order[6] could be an extreemly inefficient way to do this
                                  for(j=0; j<edges; ++j){ //find the center of the points
                                          cen[0] += tpoly[j][0];
                                          cen[1] += tpoly[j][1];
                                  } //by averaging

                                  cen[0]/= edges;
                                  cen[1]/= edges;

                                  for(j=0; j<edges; ++j){ //for each vertex
                                          theta = -10;	       //find one with largest angle from center..
                                          next = j;
                                          for ( int k= j; k<edges; ++k){
                                                  //... and check angle made between other edges
                                                  dx = tpoly[order[k]][0] - cen[0];
                                                  dy = tpoly[order[k]][1] - cen[1];
                                                  if( (dx == 0) && (dy == 0)){ //same as center?
                                                          next = k;
                                                          System.out.println("what teh ");
                                                          break; //out of this for-loop
                                                  }
                                                  tt = dy/(Math.abs(dx) + Math.abs(dy)); //else compute theta [0-4]
                                                  if( dx < 0.0 ) tt = (float)(2.0f - tt); //check quadrants 2&3
                                                  else if( dy < 0.0 ) tt = (float)(4.0f + tt); //quadrant 4
                                                  if( theta <= tt ){  //grab the max theta
                                                          next = k;
                                                          theta = tt;
                                                  }
                                          } //end for(k) angle checking
                                          // i am using 'tt' as a temp
                                          // swap polygon vertex ( is this better than another branch?)
                                          // I am not sure wich is worse: swapping 3 vectors for every edge
                                          // or: using an array to index into another array??? hmmm....
                                          //   should have payed more attention in class
                                          int tmp = order[j];
                                          order[j] = order[next];
                                          order[next] = tmp;

                                  } //end for(j) edge /angle sort

                                  gl.glBegin(GL.GL_POLYGON); //draw slice and texture map it
                                  {
                                          for(j=0; j< edges; ++j){
                                                  gl.glTexCoord3fv(tcoord[order[j]] , 0);
                                                  gl.glVertex3fv(poly[order[j]], 0);
                                          }
                                  }
                                  gl.glEnd();
                                  gl.glFlush();
                          }//end else compute convex hull
             }// end for(i) each slice

          // #ifdef WIN32
          // #if    WILDCAT
             // gl.glDisable(GL.GL_TEXTURE_COLOR_TABLE_EXT);
          // #endif
          // #else
             gl.glDisable(GL.GL_TEXTURE_COLOR_TABLE_SGI);
          // #endif
             gl.glDisable(GL.GL_BLEND);
             gl.glDisable(GL.GL_TEXTURE_3D);
             gl.glEnable(GL.GL_LIGHTING);
          }


          //========================================================================
//------------------------------------------------------------------------
//========================================================================

          public void renderSlice(GLAutoDrawable drawable, float[][] quad, float alpha)   // quad[4][3]
          {
                  switch(type){
                  case	 1: //   VolRen2DTexture:
                          System.err.println("2D texture mapping method is not implemented");
                          return;
                  case  2: //  VolRen3DTexture:
                          System.err.println("3D texture mapping method (OGL 1.2) is not implemented");
                          return;
                  case 3: // VolRen3DExt:
                          render3dSliceEXT(drawable, quad, alpha);
                          return;
                  }
          }

          //------------------------------------------------------------------------


          void render3dSliceEXT(GLAutoDrawable drawable, float[][] quad, float a)  // quad[4][3]
          {
                  GL gl = drawable.getGL();
                  gl.glDisable(GL.GL_LIGHTING); //light makes it look bad!

                  gl.glEnable(GL.GL_BLEND);
                  gl.glBlendFunc(GL.GL_ONE, GL.GL_ONE_MINUS_SRC_ALPHA);

                  gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
                  gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);

                  gl.glDisable(GL.GL_TEXTURE_COLOR_TABLE_SGI);
                  gl.glEnable(GL.GL_TEXTURE_3D);

                  gl.glBindTexture(GL.GL_TEXTURE_3D, texname3D[0]);

                  gl.glColor4f(1, 1, 1, a);
                  gl.glBegin(GL.GL_QUADS);
                  {
                          gl.glNormal3f(0,0,1);

                          gl.glTexCoord3f(quad[1][0]/sizef[0],quad[1][1]/sizef[1],quad[1][2]/sizef[2]);
                          gl.glVertex3f(quad[1][0], quad[1][1], quad[1][2]);

                          gl.glTexCoord3f(quad[0][0]/sizef[0],quad[0][1]/sizef[1],quad[0][2]/sizef[2]);
                          gl.glVertex3f(quad[0][0],quad[0][1],quad[0][2]);

                          gl.glTexCoord3f(quad[2][0]/sizef[0],quad[2][1]/sizef[1],quad[2][2]/sizef[2]);
                          gl.glVertex3f(quad[2][0],quad[2][1],quad[2][2]);

                          gl.glTexCoord3f(quad[3][0]/sizef[0],quad[3][1]/sizef[1],quad[3][2]/sizef[2]);
                          gl.glVertex3f(quad[3][0],quad[3][1],quad[3][2]);
                  }
                  gl.glEnd();

                  gl.glEnable(GL.GL_LIGHTING); //light makes it look bad!

                  gl.glDisable(GL.GL_BLEND);
          }

          //========================================================================
//------------------------------------------------------------------------
//========================================================================

          private void initVol(int v)
          {
                  size[0] = volume[v].xiSize;
                  size[1] = volume[v].yiSize;
                  size[2] = volume[v].ziSize;

                  float maxsize = Math.max(size[0], size[1]);
                  maxsize = Math.max(maxsize, size[2]);

                  //sizef [0-1] ? usualy but could be arbitrary physical units (cm, in, ft)
                  sizef[0] = volume[v].xfSize; //AFFINE(0, size[0], maxsize, 0, 1);
                  sizef[1] = volume[v].yfSize; //AFFINE(0, size[1], maxsize, 0, 1);
                  sizef[2] = volume[v].zfSize; //AFFINE(0, size[2], maxsize, 0, 1);

                  center[0] = volume[v].xfPos;
                  center[1] = volume[v].yfPos;
                  center[2] = volume[v].zfPos;
           }


           //------------------------------------------------------------------------

           private int glVolErr(GLAutoDrawable drawable, String place)
           {
                   GL gl = drawable.getGL();
                   int errCode;
                   String errString;

                   if((errCode = gl.glGetError()) != GL.GL_NO_ERROR){
                           errString = glu.gluErrorString(errCode);
                           System.err.println("OpenGL error : VolumeRenderer::" + place + " : "
                                   + errString);
                           return 1;
                   }
                   return 0;
           }

           //------------------------------------------------------------------------

           public void renderBBox(GLAutoDrawable drawable)
           {
                   GL gl = drawable.getGL();
                   //glDisable(GL_LIGHTING);
                   gl.glDisable(GL.GL_TEXTURE_3D);
                   gl.glDisable(GL.GL_TEXTURE_2D);
                   gl.glDisable(GL.GL_TEXTURE_3D);
                   gl.glDisable(GL.GL_LIGHTING);

                   gl.glPolygonMode(GL.GL_FRONT, GL.GL_LINE);
                   gl.glPolygonMode(GL.GL_BACK, GL.GL_LINE);
                   gl.glLineWidth(3);

                   //glBindTexture(GL_TEXTURE_2D, 0);
                   gl.glColor3f(.35f, 0f, .1f);
                   gl.glBegin(GL.GL_QUADS);
                   {
                           gl.glVertex3f(0, 0, 0);
                           gl.glVertex3f(1, 0, 0);
                           gl.glVertex3f(1, 1, 0);
                           gl.glVertex3f(0, 1, 0);

                           gl.glVertex3f(0, 0, 1);
                           gl.glVertex3f(1, 0, 1);
                           gl.glVertex3f(1, 1, 1);
                           gl.glVertex3f(0, 1, 1);

                           gl.glVertex3f(0, 0, 0);
                           gl.glVertex3f(0, 0, 1);
                           gl.glVertex3f(0, 1, 1);
                           gl.glVertex3f(0, 1, 0);

                           gl.glVertex3f(1, 0, 0);
                           gl.glVertex3f(1, 0, 1);
                           gl.glVertex3f(1, 1, 1);
                           gl.glVertex3f(1, 1, 0);
                   }
                   gl.glEnd();

                   gl.glLineWidth(1);

                   gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
                   gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
                   gl.glEnable(GL.GL_LIGHTING);
            }

            //------------------------------------------------------------------------

            private void renderBBox(GLAutoDrawable drawable, float xb, float yb, float zb)
            {
                    GL gl = drawable.getGL();
                    //glDisable(GL_LIGHTING);
                    gl.glDisable(GL.GL_TEXTURE_3D);
                    gl.glDisable(GL.GL_TEXTURE_2D);
                    gl.glDisable(GL.GL_TEXTURE_3D);
                    gl.glDisable(GL.GL_LIGHTING);

                    gl.glPolygonMode(GL.GL_FRONT, GL.GL_LINE);
                    gl.glPolygonMode(GL.GL_BACK, GL.GL_LINE);
                    gl.glLineWidth(3);

                    //glBindTexture(GL_TEXTURE_2D, 0);
                    gl.glColor3f(.35f, 0f, .1f);

                    gl.glBegin(GL.GL_QUADS);
                    {
                            gl.glVertex3f(0, 0, 0);
                            gl.glVertex3f(xb, 0, 0);
                            gl.glVertex3f(xb, yb, 0);
                            gl.glVertex3f(0, yb, 0);

                            gl.glVertex3f(0, 0, zb);
                            gl.glVertex3f(xb, 0, zb);
                            gl.glVertex3f(xb, yb, zb);
                            gl.glVertex3f(0, yb, zb);

                            gl.glVertex3f(0, 0, 0);
                            gl.glVertex3f(0, 0, zb);
                            gl.glVertex3f(0, yb, zb);
                            gl.glVertex3f(0, yb, 0);

                            gl.glVertex3f(xb, 0, 0);
                            gl.glVertex3f(xb, 0, zb);
                            gl.glVertex3f(xb, yb, zb);
                            gl.glVertex3f(xb, yb, 0);
                    }
                    gl.glEnd();

                    gl.glLineWidth(1);

                    gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
                    gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
                    gl.glEnable(GL.GL_LIGHTING);
            }

            //--------------------------------------------------------------------------
            private void renderBBoxBrackets(GLAutoDrawable drawable, float xb, float yb, float zb)
            {
                    GL gl = drawable.getGL();
                    //glDisable(GL_LIGHTING);
                    //glDisable(GL_TEXTURE_3D);
                    gl.glDisable(GL.GL_TEXTURE_2D);
                    gl.glDisable(GL.GL_TEXTURE_3D);
                    gl.glDisable(GL.GL_LIGHTING);

                    gl.glPolygonMode(GL.GL_FRONT, GL.GL_LINE);
                    gl.glPolygonMode(GL.GL_BACK, GL.GL_LINE);
                    gl.glLineWidth(3);

                    gl.glColor3f(.35f, 0f, .1f);

                    drawLine(drawable, 0, 0, 0,   0+xb/10, 0, 0);
                    drawLine(drawable, 0, 0, 0,   0, 0+yb/10, 0);
                    drawLine(drawable, 0, 0, 0,   0, 0, 0+zb/10);

                    drawLine(drawable, xb, 0, 0,	xb-xb/10, 0, 0);
                    drawLine(drawable, xb, 0, 0,	xb, 0+yb/10, 0);
                    drawLine(drawable, xb, 0, 0,	xb, 0, 0+zb/10);

                    drawLine(drawable, 0, yb, 0,	0+xb/10, yb, 0);
                    drawLine(drawable, 0, yb, 0,	0, yb-yb/10, 0);
                    drawLine(drawable, 0, yb, 0,	0, yb, 0+zb/10);

                    drawLine(drawable, 0, 0, zb,	0+xb/10, 0, zb);
                    drawLine(drawable, 0, 0, zb,	0, 0+yb/10, zb);
                    drawLine(drawable, 0, 0, zb,	0, 0, zb-zb/10);

                    drawLine(drawable, xb, yb, 0,	 xb-xb/10, yb, 0);
                    drawLine(drawable, xb, yb, 0,	 xb, yb-yb/10, 0);
                    drawLine(drawable, xb, yb, 0,	 xb, yb, 0+zb/10);

                    drawLine(drawable, xb, 0, zb,	 xb-xb/10, 0, zb);
                    drawLine(drawable, xb, 0, zb,	 xb, 0+yb/10, zb);
                    drawLine(drawable, xb, 0, zb,	 xb, 0, zb-zb/10);

                    drawLine(drawable, 0, yb, zb,	 0+xb/10, yb, zb);
                    drawLine(drawable, 0, yb, zb,	 0, yb-yb/10, zb);
                    drawLine(drawable, 0, yb, zb,	 0, yb, zb-zb/10);

                    drawLine(drawable, xb, yb, zb,	  xb-xb/10, yb, zb);
                    drawLine(drawable, xb, yb, zb,	  xb, yb-yb/10, zb);
                    drawLine(drawable, xb, yb, zb,	  xb, yb, zb-zb/10);

                    gl.glLineWidth(1);

                    gl.glPolygonMode(GL.GL_FRONT, GL.GL_FILL);
                    gl.glPolygonMode(GL.GL_BACK, GL.GL_FILL);
                    gl.glEnable(GL.GL_LIGHTING);
          }


          //========================================================================
//--------------------------------------------------------------------------
//========================================================================
/*
          #if USE_NRRD
          int VolumeRenderer::loadNRRD_Volume(const char *volname)
          {
                  FILE *fin;
                  char *err;
                  //int axis, pos;
                  Nrrd *nin;

                  //get file handle for the volume we want to read
                  if (!(fin = fopen(volname, "r"))){
                          cerr << "VolumeRenderer.cpp : loadNRRD_Volume() : Couldn't open : "
                                  << volname
                                  << " for reading\n";
                          return 1;
                  }

                  //get nrrd handle for the volume
                  if (!(nin = nrrdNewRead(fin))) {
                          err = nrrdStrdupErr();
                          cerr << "genVol.cc : loadNRRD_Volume() : error reading nrrd : "
                                  << err << "\n";
                          return 1;
                  }

                  //note the hack here: assumes that the type is always uchar!!!!!!
                  //also assumes that the texture is always a power of two.
                  //conversion routiens do exist to fix this-
                  nrrd_vol = nrrd_uchar = nin;

                  data = (unsigned char*) nrrd_uchar->data;
                  size[0] = nrrd_uchar->size[0];
                  size[1] = nrrd_uchar->size[1];
                  size[2] = nrrd_uchar->size[2];

                  cerr << "Successfuly read " << volname << endl;
                  return 0;
          }
#endif
*/

//==========================================================================
//---------- Inlines -------------------------------------------------------
//==========================================================================

      // GLfloat out[3], GLdouble mat[16], GLfloat in[3])
      private void translateV3(float[] out, double[] mat, float[] in)
      {
        out[0] = (float)(mat[0]*in[0]) + (float)(mat[4]*in[1]) + (float)(mat[8]* in[2]);// + mat[12];
        out[1] = (float)(mat[1]*in[0]) + (float)(mat[5]*in[1]) + (float)(mat[9]* in[2]);// + mat[13];
        out[2] = (float)(mat[2]*in[0]) + (float)(mat[6]*in[1]) + (float)(mat[10]*in[2]);// + mat[14];
      }
//--------------------------------------------------------------------------
// float one[4], float two[4]
      private float dotV3(float[] one, float[] two)
      {
        return one[0]*two[0] + one[1]*two[1] + one[2]*two[2];
      }
//--------------------------------------------------------------------------
// GLdouble m[16]
      private void identityMatrix(double[] m)
      {
        m[0]=1; m[4]=0; m[8]= 0; m[12]=0;
        m[1]=0; m[5]=1; m[9]= 0; m[13]=0;
        m[2]=0; m[6]=0; m[10]=1; m[14]=0;
        m[3]=0; m[7]=0; m[11]=0; m[15]=1;
      }

      //--------------------------------------------------------------------------
      private int intersect(   // array[3]
               float[] p0, float[] p1, //line end points
               float[] t0, float[] t1, //texture points
               float[] v0, float[] v1, //view coord points
               float[] sp, float[] sn, //plane point & norm
               float[] pnew, float[] tnew, float[] vnew) //new values
      {
              //t = (sn.(sp - p0))/(sn.(p1 - p0))
              float t = ((sn[0]*(sp[0] - p0[0]) + sn[1]*(sp[1] - p0[1])
                     + sn[2]*(sp[2] - p0[2])) /
                         (sn[0]*(p1[0] - p0[0]) + sn[1]*(p1[1] - p0[1])
                         + sn[2]*(p1[2] - p0[2])));
              //note if the denominator is zero t is a NAN so we should have no problems?

              if( (t>=0) && (t<=1) ){
                      //compute line intersection
                      pnew[0] = p0[0] + t*(p1[0] - p0[0]);
                      pnew[1] = p0[1] + t*(p1[1] - p0[1]);
                      pnew[2] = p0[2] + t*(p1[2] - p0[2]);
                      //compute texture interseciton
                      tnew[0] = t0[0] + t*(t1[0] - t0[0]);
                      tnew[1] = t0[1] + t*(t1[1] - t0[1]);
                      tnew[2] = t0[2] + t*(t1[2] - t0[2]);
                      //compute view coordinate intersections
                      vnew[0] = v0[0] + t*(v1[0] - v0[0]);
                      vnew[1] = v0[1] + t*(v1[1] - v0[1]);
                      vnew[2] = v0[2] + t*(v1[2] - v0[2]);
                      return 1;
              }
              return 0;
      }

      //--------------------------------------------------------------------------
      // GLdouble invm[16], const GLdouble m[16]
      private void inverseMatrix( double[] invm, double[] m )
      {
              float det =
                      (float)(m[0]*m[5]*m[10])-
                      (float)(m[0]*m[6]*m[9])-
                      (float)(m[1]*m[4]*m[10])+
                      (float)(m[1]*m[6]*m[8])+
                      (float)(m[2]*m[4]*m[9])-
                      (float)(m[2]*m[5]*m[8]);

              invm[0] = (m[5]*m[10]-m[6]*m[9])/det;
              invm[1] = (-m[1]*m[10]+m[2]*m[9])/det;
              invm[2] = (m[1]*m[6]-m[2]*m[5])/det;
              invm[3] = 0.0;

              invm[4] = (-m[4]*m[10]+m[6]*m[8])/det;
              invm[5] = (m[0]*m[10]-m[2]*m[8])/det;
              invm[6] = (-m[0]*m[6]+m[2]*m[4])/det;
              invm[7] = 0.0;

              invm[8] = (m[4]*m[9]-m[5]*m[8])/det;
              invm[9] = (-m[0]*m[9]+m[1]*m[8])/det;
              invm[10] = (m[0]*m[5]-m[1]*m[4])/det;
              invm[11] = 0.0;

              invm[12] = (-m[4]*m[9]*m[14]+m[4]*m[13]*m[10]+
                     m[5]*m[8]*m[14]-m[5]*m[12]*m[10]-
                         m[6]*m[8]*m[13]+m[6]*m[12]*m[9])/det;
              invm[13] = (m[0]*m[9]*m[14]-m[0]*m[13]*m[10]-
                     m[1]*m[8]*m[14]+m[1]*m[12]*m[10]+
                         m[2]*m[8]*m[13]-m[2]*m[12]*m[9])/det;
              invm[14] = (-m[0]*m[5]*m[14]+m[0]*m[13]*m[6]+
                     m[1]*m[4]*m[14]-m[1]*m[12]*m[6]-
                         m[2]*m[4]*m[13]+m[2]*m[12]*m[5])/det;
              invm[15] = 1.0;
      }

      //--------------------------------------------------------------------------
      private void printV(float[] v){
              System.err.println(v[0] + ", " + v[1] + ", " + v[2]);
      }
//--------------------------------------------------------------------------
      private void drawLine(GLAutoDrawable drawable, float x0, float y0, float z0,
                            float x1, float y1, float z1)
      {
              GL gl = drawable.getGL();
              gl.glBegin(GL.GL_LINES);
              gl.glVertex3f(x0, y0, z0);
              gl.glVertex3f(x1, y1, z1);
              gl.glEnd();
      }

//--------------------------------------------------------------------------
      //be sure to load the
       public TLUT getColorMap(){return tlut;}   //color map that you want, the
       //default is linear grey scale

// (non-0)=on, 0=off (default = off)
       public void useBBox(int on_off){m_bb= on_off;}
       public void useBBoxBrackets(int on_off){m_bbb= on_off;}


       public int createTLUT()
       {
               if(tlut == null)
                       tlut = new TLUT();
               return 1;
       }


}
