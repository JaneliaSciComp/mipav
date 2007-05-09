package gov.nih.mipav.view.multihisto.src;

import javax.media.opengl.*;
import java.nio.*;
import javax.media.opengl.glu.*;

public class NV20VolRen extends gluvvPrimitive {

  // private PBuffer pbuff;
  private GLU glu = new GLU();
  private int[] texNames2D;
  private int[] shadeNames;
  private int[]  deptexName = new int[1];
  private byte[] deptex;
  private byte[] gDeptex;
  private byte[] iDeptex;
  private int[]   temptexNames = new int[3]; //one for each of the axies [z-y-x]

  private int sx, sy, sz;
  private float lastSamp;
  private float lastGoodSamp;
  private float lastInteSamp;

  private double[] mv0 = new double[16];

  private float[][]  vcoords = new float[8][3];  //vertex coordinates of volume
  private float[][]  ccoords = new float[8][3];  //clip space coords

  private float[][]  texts = new float[3][2];    //extents for *small volume* and ortho clipping
  private float[][]  vexts = new float[3][2];

  private VectorMath math = new VectorMath();

  private gluvvGlobal gluvv;


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


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

  public NV20VolRen(gluvvGlobal _gluvv)
  {
    gluvv = _gluvv;
          texNames2D = null;
          sx = sy = sz = 0;

          if(gluvv.mv != null ){
                  vcoords[0][0] = vcoords[0][1] = vcoords[0][2] = 0;
                  vcoords[1][0] = gluvv.mv.xfSize; vcoords[1][1] = vcoords[1][2] = 0;
                  vcoords[2][0] = 0; vcoords[2][1] = gluvv.mv.yfSize; vcoords[2][2] = 0;
                  vcoords[3][0] = gluvv.mv.xfSize; vcoords[3][1] = gluvv.mv.yfSize; vcoords[3][2] = 0;
                  vcoords[4][0] = vcoords[4][1] = 0; vcoords[4][2] = gluvv.mv.zfSize;
                  vcoords[5][0] = gluvv.mv.xfSize; vcoords[5][1] = 0; vcoords[5][2] = gluvv.mv.zfSize;
                  vcoords[6][0] = 0; vcoords[6][1] = gluvv.mv.yfSize; vcoords[6][2] = gluvv.mv.zfSize;
                  vcoords[7][0] = gluvv.mv.xfSize;
                  vcoords[7][1] = gluvv.mv.yfSize; vcoords[7][2] = gluvv.mv.zfSize;

                  texts[0][0] = texts[1][0] = texts[2][0] = 0;
                  texts[0][1] = texts[1][1] = texts[2][1] = 1;
                  vexts[0][0] = vexts[1][0] = vexts[2][0] = 0;
                  vexts[0][1] = gluvv.mv.xfSize;
                  vexts[1][1] = gluvv.mv.yfSize;
                  vexts[2][1] = gluvv.mv.zfSize;
          }
  }

  //================================================================= INIT
//======================================================================

  public void init(GLAutoDrawable drawable)
  {
     GL gl = drawable.getGL();
          if(gluvv.mv != null){
                  create2DTextures(drawable, gluvv.mv);
                  create2DDepTex(drawable);
                  createTempTex(drawable);
                  // pbuff ???????????????????????????????????????????????????????????????????????????????
                  //we should select pbuffer size based on the size of the largest slice extents
                  /*
                  if(!(pbuff = new PBuffer(512, 512, GLUT_SINGLE | GLUT_RGBA))){
                          System.err.println("ERROR: PBuffer intialization failed!");
                  } else {
                          pbuff.Initialize(true);
                          pbuff.MakeCurrent();
                          gl.glClearColor( 0.0f, 0.0f, 0.0f, 0.0f );
                          gl.glMatrixMode( GL.GL_PROJECTION );
                          gl.glOrtho(0,512,0,512,1,3);
                          gl.glMatrixMode(GL.GL_MODELVIEW);
                          glu.gluLookAt(0,0,2,
                                    0,0,0,
                                    0,1,0);
                          MakeCurrent();
                          System.err.println("PBuffer intialization succeded");
                  }
                  */
          }
  }

  //=============================================================== DRAW !
//======================================================================

  public void draw(GLAutoDrawable drawable)
  {
          if(texNames2D == null) return;
          //interpSlices();
          //drawSlices();
          drawVolume(drawable);

  }


  //========================================================== Draw Volume
//======================================================================

  private void drawVolume(GLAutoDrawable drawable)
  {
          //double mv0[16];
          GL gl = drawable.getGL();
          gl.glPushMatrix();{
                  //--------------- set up transformation -------------------------------
          gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, mv0, 0);

          gl.glTranslatef(gluvv.rinfo.trans[0], //translate
                       gluvv.rinfo.trans[1],
                       gluvv.rinfo.trans[2]);
          gl.glMultMatrixf(gluvv.rinfo.xform, 0);  //rotate
          gl.glTranslatef(-gluvv.mv.xfSize/2f,  //center
                       -gluvv.mv.yfSize/2f,
                       -gluvv.mv.zfSize/2f);

          double[] mv = new double[16];
          gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, mv, 0);  //save modelview matrix
          //--------------- end set up transformation ----------------------------

          //----------------------------- get clip space values -----------------
          float[] trans = {
              1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 1, 0,
              gluvv.rinfo.trans[0], gluvv.rinfo.trans[1], gluvv.rinfo.trans[2],
              1};
          float[] shift = {
              1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 1, 0,
              -gluvv.mv.xfSize / 2f, -gluvv.mv.yfSize / 2f,
              -gluvv.mv.zfSize / 2f, 1f};
          float[] tmp1 = new float[16];
          math.matrixMult(tmp1, gluvv.rinfo.xform, shift);
          float[] volmv = new float[16]; //volume - model view
          math.matrixMult(volmv, trans, tmp1);

          float[] clipt = {
              1, 0, 0, 0,
              0, 1, 0, 0,
              0, 0, 1, 0,
              gluvv.clip.pos[0], gluvv.clip.pos[1], gluvv.clip.pos[2], 1};
          float[] tmp2 = new float[16];
          math.matrixMult(tmp2, clipt, gluvv.clip.xform);
          float[] invcp = new float[16]; // inverse clip
          math.inverseMatrix(invcp, tmp2);

          float[] vol2cp = new float[16]; //volume space to clip space
          math.matrixMult(vol2cp, invcp, volmv);

          for (int i = 0; i < 8; ++i) {
            math.translateV3W(ccoords[i], vol2cp, vcoords[i]);
          }
          //-------------------------- end get clip space values -----------------

          //-------------- Re-Scale Alpha values ---------------------------------
          if (gluvv.volren.scaleAlphas == 1) {
            if ( (lastSamp != gluvv.volren.sampleRate) || (gluvv.volren.loadTLUT == 1)) { //see if the sample rate changed
              if ( (lastGoodSamp != gluvv.volren.goodSamp) || gluvv.volren.loadTLUT == 1) {
                 copyScale(gluvv.volren.goodSamp * 1 / gluvv.volren.gamma, gDeptex);
                lastGoodSamp = gluvv.volren.goodSamp;
              }
              if ( (lastInteSamp != gluvv.volren.interactSamp) || gluvv.volren.loadTLUT == 1) {
                copyScale(gluvv.volren.interactSamp * 1 / gluvv.volren.gamma, iDeptex);
                lastInteSamp = gluvv.volren.interactSamp;
              }

              if (gluvv.volren.sampleRate == gluvv.volren.goodSamp) {
                loadDepTex(drawable, gDeptex);
                lastSamp = gluvv.volren.goodSamp;
              }
              else if (gluvv.volren.sampleRate == gluvv.volren.interactSamp) {
                loadDepTex(drawable, iDeptex);
                lastSamp = gluvv.volren.interactSamp;
              }
              gluvv.volren.loadTLUT = 0;
            }
          }
          else { //just do gamma scale
            if (gluvv.volren.loadTLUT == 1) {
              copyScale(1 / gluvv.volren.gamma, gDeptex);
              loadDepTex(drawable, gDeptex);
              gluvv.volren.loadTLUT = 0;
            }
          }
          //-------------- end Re-Scale Alpha values ---------------------------------

          //-------------- clipping plane setup -----------
          if ((gluvv.clip.on == 1) && (gluvv.clip.ortho == 0)) {
            gl.glPushMatrix();
            {
              gl.glLoadIdentity();
              gl.glMultMatrixd(mv0, 0);
              gl.glTranslatef(gluvv.clip.pos[0], //location of clipping plane
                           gluvv.clip.pos[1],
                           gluvv.clip.pos[2]);
              gl.glMultMatrixf(gluvv.clip.xform, 0); //rotation of clip plane
              double[] zup = {0, 0, -1, 0}; // always in z direction
              gl.glEnable(GL.GL_CLIP_PLANE5); //enable the gl clip plane
              gl.glClipPlane(GL.GL_CLIP_PLANE5, zup, 0);
            }
            gl.glPopMatrix();
          }

          texts[0][0] = texts[1][0] = texts[2][0] = 0.02f;
          texts[0][1] = texts[1][1] = texts[2][1] = .98f;
          vexts[0][0] = vexts[1][0] = vexts[2][0] = 0f;
          vexts[0][1] = gluvv.mv.xfSize;
          vexts[1][1] = gluvv.mv.yfSize;
          vexts[2][1] = gluvv.mv.zfSize;

          if (gluvv.clip.on == 1 && gluvv.clip.ortho == 1) {
            float[] cp = new float[3];
            cp[0] = gluvv.clip.vpos[0] > 0 ?
                (gluvv.clip.vpos[0] < gluvv.mv.xfSize ? gluvv.clip.vpos[0] : gluvv.mv.xfSize) : 0;
            cp[1] = gluvv.clip.vpos[1] > 0 ?
                (gluvv.clip.vpos[1] < gluvv.mv.yfSize ? gluvv.clip.vpos[1] : gluvv.mv.yfSize) : 0;
            cp[2] = gluvv.clip.vpos[2] > 0 ?
                (gluvv.clip.vpos[2] < gluvv.mv.zfSize ? gluvv.clip.vpos[2] : gluvv.mv.zfSize) : 0;
            if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ) {
                texts[0][1] = cp[0] / gluvv.mv.xfSize;
                vexts[0][1] = cp[0];
            } else if (  gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg ) {
                texts[0][0] = cp[0] / gluvv.mv.xfSize;
                vexts[0][0] = cp[0];
            } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ) {
                texts[1][1] = cp[1] / gluvv.mv.yfSize;
                vexts[1][1] = cp[1];
            } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg ) {
                texts[1][0] = cp[1] / gluvv.mv.yfSize;
                vexts[1][0] = cp[1];
            } else if (gluvv.clip.oaxis ==  VolRenMajorAxis.VolRenAxisZPos ) {
                texts[2][1] = cp[2] / gluvv.mv.zfSize;
                vexts[2][1] = cp[2];
            } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg ) {
                texts[2][0] = cp[2] / gluvv.mv.zfSize;
                vexts[2][0] = cp[2];
            }
          }

          //-------------- end clipping plane setup -------


          gl.glEnable(GL.GL_BLEND);
          gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

          //-------------- draw Clip plane slice ----------
          float[] vdir = new float[3];
          math.subV3(vdir, gluvv.env.eye, gluvv.clip.pos);
          math.normalizeV3(vdir);
          math.normalizeV3(gluvv.clip.dir);
          float dv = math.dotV3(vdir, gluvv.clip.dir);
          drawClip(drawable, dv);
          //--------------  end draw Clip plane slice -----


          gl.glEnable(GL.GL_BLEND);
          gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

          //-------------- Texture Shader setup --------------------------------------
          gl.glEnable(GL.GL_TEXTURE_SHADER_NV);
          {
            GlErr(drawable, "nv20volren", " enable shader");

            gl.glActiveTexture(GL.GL_TEXTURE0);
            { //this is for the slice
              gl.glEnable(GL.GL_TEXTURE_2D);
              gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
              gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV,
                        GL.GL_TEXTURE_2D);
            }
            gl.glActiveTexture(GL.GL_TEXTURE1);
            { //this is for the transfer function
              gl.glEnable(GL.GL_TEXTURE_2D);
              gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
              gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_DEPENDENT_GB_TEXTURE_2D_NV);
              gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_PREVIOUS_TEXTURE_INPUT_NV, GL.GL_TEXTURE0);
              gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
            }
            gl.glActiveTexture(GL.GL_TEXTURE2);
            { // for shading or clipping (not both)
              gl.glDisable(GL.GL_TEXTURE_2D);
              gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
              gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
              if ( (gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)) {
                gl.glEnable(GL.GL_TEXTURE_2D);
                gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
              }
              else if ( (gluvv.clip.on == 1) && (gluvv.clip.ortho == 0))
                gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_CULL_FRAGMENT_NV);
            }
            gl.glActiveTexture(GL.GL_TEXTURE3);
            { //not used
              gl.glDisable(GL.GL_TEXTURE_2D);
              gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
              gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
              if ( (gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)) {
                gl.glEnable(GL.GL_TEXTURE_2D);
                gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
              }
            }
            //-------------- Texture Shader setup --------------------------------------

            //-------------- Register Combiner Setup -----------------------------------
            setupShading(drawable);
            //-------------- end Register Combiner Setup -------------------------------

            //-------------- Render the Volume -----------------------------------------
            int ma = getMajorAxis(mv);

            gl.glColor4f(1, 1, 1, gluvv.tf.slider1hi);
            if ( ma  == VolRenMajorAxis.VolRenAxisXPos ) {
                renXPos(drawable);
            } else if ( ma == VolRenMajorAxis.VolRenAxisYPos ) {
                renYPos(drawable);
            } else if ( ma == VolRenMajorAxis.VolRenAxisZPos ) {
                renZPos(drawable);
            } else if ( ma == VolRenMajorAxis.VolRenAxisXNeg ) {
                renXNeg(drawable);
            } else if ( ma == VolRenMajorAxis.VolRenAxisYNeg ) {
                renYNeg(drawable);
            } else if ( ma == VolRenMajorAxis.VolRenAxisZNeg ) {
                renZNeg(drawable);
            } //end switch major axis
            //-------------- end Render the Volume --------------------------------------

          }
          gl.glDisable(GL.GL_TEXTURE_SHADER_NV);

          //if((gl.gluvv.shade == gl.gluvvShadeDiff) || (gl.gluvv.shade == gl.gluvvShadeDSpec)){
          gl.glDisable(GL.GL_REGISTER_COMBINERS_NV);
          //}

          //---------------- Reset the texture units ------------------------------------
          gl.glActiveTexture(GL.GL_TEXTURE3);
          { //not used
            gl.glDisable(GL.GL_TEXTURE_2D);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
          }
          gl.glActiveTexture(GL.GL_TEXTURE2);
          { //not used
            gl.glDisable(GL.GL_TEXTURE_2D);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
          }
          gl.glActiveTexture(GL.GL_TEXTURE1);
          { //not used
            gl.glDisable(GL.GL_TEXTURE_2D);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
          }
          gl.glActiveTexture(GL.GL_TEXTURE0);
          { //not used
            gl.glDisable(GL.GL_TEXTURE_2D);
            gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
            gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
          }
          //---------------- end Reset the texture units --------------------------------

          //-------------- draw Clip plane slice ----------
          gl.glEnable(GL.GL_BLEND);
          gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
          drawClip(drawable, -dv);
          gl.glDisable(GL.GL_BLEND);
          //-------------- end draw Clip plane slice ------

        }
        gl.glPopMatrix();

        gl.glDisable(GL.GL_CLIP_PLANE5);

        }

        //======================================================= Get Major Axis
//======================================================================

        private int getMajorAxis(double[] mv)
        {
           double[] zdir = {0, 0, -1, 1};
           double[] xdir = {1, 0, 0, 1};
           double[] ydir = {0, 1, 0, 1};
           double[] newzdir = new double[4];
           double[] newxdir = new double[4];
           double[] newydir = new double[4];
           double dotz, dotx, doty;

           mv[12]=0; mv[13]=0; mv[14]=0;
           math.translateV3(newzdir, mv, zdir);
           math.translateV3(newxdir, mv, xdir);
           math.translateV3(newydir, mv, ydir);

           dotz = math.dotV3(newzdir, zdir);
           dotx = math.dotV3(newxdir, zdir);
           doty = math.dotV3(newydir, zdir);

           if((dotz > dotx) && (dotz > doty) && (dotz > -doty) && (dotz > -dotx)){
              return VolRenMajorAxis.VolRenAxisZPos;// positve z axis
           }
           else if((dotx > doty) && (dotx > -doty) && (dotx > -dotz)){
              return VolRenMajorAxis.VolRenAxisXPos; // positive x axis
           }
           else if((doty > 0) && (doty > -dotx) && (doty > -dotz)){
              return VolRenMajorAxis.VolRenAxisYPos; // positive y axis
           }
           else if((-dotz > 0) && (-dotz > -dotx) && (-dotz > -doty)){
              return VolRenMajorAxis.VolRenAxisZNeg; // negative z axis
           }
           else if((-dotx > 0) && (-dotx > -doty)){
              return VolRenMajorAxis.VolRenAxisXNeg; // negativ x axis
           }
           else if(-doty > 0){
              return VolRenMajorAxis.VolRenAxisYNeg;// negative y axis
           }
           else{
              System.err.println("VolumeRenderer Error: getMajorAxis(): viewing direction unknown\n");
              return VolRenMajorAxis.VolRenAxisUnknown;
           }
       }

       //=================================================== Render along axies
//======================================================================

       private void renXPos(GLAutoDrawable drawable)
       {
               GL gl = drawable.getGL();
               float dist = vexts[0][1] - vexts[0][0];

               int tsamps = (int)(gluvv.volren.sampleRate * 100 * gluvv.mv.xfSize);
               int samps = (int)(tsamps * (texts[0][1] - texts[0][0]));

               //float inc = gluvv.mv.zfSize/(float)samps;
               float inc = dist/(float)samps;

               float idxi = (float)((1.0f/(float)samps) * sx * (dist/gluvv.mv.xfSize));
               int   istart = (int)(texts[0][0] * sx);

               float[] w = new float[4];

               for(int i=samps-2; i>=1; --i){
                       float ds = (float)(1.0f - ((istart +i*idxi) - (int)(istart + i*idxi)));
                       w[0] = ds; w[1] = ds; w[2] = ds; w[3] = ds;
                       interpSlice(drawable, temptexNames[2],sy,sz,
                                   texNames2D[sz+sy+(int)(istart + i*idxi)],
                                   texNames2D[sz+sy+Math.min((int)(istart + i*idxi)+1, sx-1)], w);
                       gl.glActiveTexture(GL.GL_TEXTURE0); gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[2]);
                       gl.glActiveTexture(GL.GL_TEXTURE2); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+sy+(int)(istart + i*idxi)]);
                       gl.glActiveTexture(GL.GL_TEXTURE3); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+sy+Math.min((int)(istart + i*idxi)+1, sx-1)]);

                       w[3] = .3f;
                       gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w, 0); //set w

                       gl.glBegin(GL.GL_QUADS);{
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][0],texts[2][0]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][0], vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][1],texts[2][0]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][1], vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][1],texts[2][1]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][1], vexts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][0],texts[2][1]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][0], vexts[2][1]);
                       } gl.glEnd();
               }
       }

       private void renXNeg(GLAutoDrawable drawable)
       {
          GL gl = drawable.getGL();
               float dist = vexts[0][1] - vexts[0][0];

               int tsamps = (int)(gluvv.volren.sampleRate * 100 * gluvv.mv.xfSize);
               int samps = (int)(tsamps * (texts[0][1] - texts[0][0]));

               //float inc = gluvv.mv.zfSize/(float)samps;
               float inc = dist/(float)samps;

               float idxi = (float)((1.0f/(float)samps) * sx * (dist/gluvv.mv.xfSize));
               int   istart = (int)(texts[0][0] * sx);

               float[] w = new float[4];


               for(int i=1; i<samps-1; ++i){
                       float ds = (float)(1.0f - ((istart +i*idxi) - (int)(istart + i*idxi)));
                       w[0] = ds; w[1] = ds; w[2] = ds; w[3] = ds;
                       interpSlice(drawable, temptexNames[2],sy,sz,
                                   texNames2D[sz+sy+(int)(istart + i*idxi)],
                                   texNames2D[sz+sy+Math.min((int)(istart + i*idxi)+1, sx-1)], w);
                       gl.glActiveTexture(GL.GL_TEXTURE0); gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[2]);
                       gl.glActiveTexture(GL.GL_TEXTURE2); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+sy+(int)(istart + i*idxi)]);
                       gl.glActiveTexture(GL.GL_TEXTURE3); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+sy+Math.min((int)(istart + i*idxi)+1, sx-1)]);


                       w[3] = .3f;
                       gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w, 0); //set w

                       gl.glBegin(GL.GL_QUADS);{
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][0],texts[2][0]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][0], vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][1],texts[2][0]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][1], vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][1],texts[2][1]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][1], vexts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[1][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[1][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[1][0],texts[2][1]);
                               gl.glVertex3f(vexts[0][0] + i*inc, vexts[1][0], vexts[2][1]);
                       } gl.glEnd();

               }

       }

       private void renYPos(GLAutoDrawable drawable)
       {
         GL gl = drawable.getGL();
               float dist = vexts[1][1] - vexts[1][0];

               int tsamps = (int)(gluvv.volren.sampleRate * 100 * gluvv.mv.yfSize);
               int samps = (int)(tsamps * (texts[1][1] - texts[1][0]));

               //float inc = gluvv.mv.zfSize/(float)samps;
               float inc = dist/(float)samps;

               float idxi = (float)((1.0f/(float)samps) * sy * (dist/gluvv.mv.yfSize));
               int   istart = (int)(texts[1][0] * sy);
               float[] w = new float[4];


               for(int i=samps-2; i>=1; --i){
                       float ds = (float)(1.0f - ((istart + i*idxi) - (int)(istart + i*idxi)));
                       w[0] = ds; w[1] = ds; w[2] = ds; w[3] = ds;
                       interpSlice(drawable, temptexNames[1],sx,sz,
                                   texNames2D[sz+(int)(istart + i*idxi)],
                                   texNames2D[sz+Math.min((int)(istart + i*idxi)+1, sy-1)], w);
                       w[3] = .3f;
                       gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w, 0); //set w

                       gl.glActiveTexture(GL.GL_TEXTURE0); gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[1]);
                       gl.glActiveTexture(GL.GL_TEXTURE2); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+(int)(istart + i*idxi)]);
                       gl.glActiveTexture(GL.GL_TEXTURE3); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+Math.min((int)(istart + i*idxi)+1, sy-1)]);

                       gl.glBegin(GL.GL_QUADS);{
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[2][0]);
                               gl.glVertex3f(vexts[0][0],vexts[1][0]+i*inc, vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[2][0]);
                               gl.glVertex3f(vexts[0][1],vexts[1][0]+i*inc, vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[2][1]);
                               gl.glVertex3f(vexts[0][1],vexts[1][0]+i*inc, vexts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[2][1]);
                               gl.glVertex3f(vexts[0][0],vexts[1][0]+i*inc, vexts[2][1]);
                       } gl.glEnd();

               }
       }

       private void renYNeg(GLAutoDrawable drawable)
       {
       GL gl = drawable.getGL();
               float dist = vexts[1][1] - vexts[1][0];

               int tsamps = (int)(gluvv.volren.sampleRate * 100 * gluvv.mv.yfSize);
               int samps = (int)(tsamps * (texts[1][1] - texts[1][0]));

               //float inc = gluvv.mv.zfSize/(float)samps;
               float inc = dist/(float)samps;

               float idxi = (float)((1.0/(float)samps) * sy * (dist/gluvv.mv.yfSize));
               int   istart = (int)(texts[1][0] * sy);
               float[] w = new float[4];


               for(int i=1; i<samps-2; ++i){
                       float ds = (float)(1.0f - ((istart + i*idxi) - (int)(istart + i*idxi)));
                       w[0] = ds; w[1] = ds; w[2] = ds; w[3] = ds;
                       interpSlice(drawable, temptexNames[1],sx,sz,
                                   texNames2D[sz+(int)(istart + i*idxi)],
                                   texNames2D[sz+Math.min((int)(istart + i*idxi)+1, sy-1)], w);

                       w[3] = .3f;
                       gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w, 0); //set w

                       gl.glActiveTexture(GL.GL_TEXTURE0); gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[1]);
                       gl.glActiveTexture(GL.GL_TEXTURE2); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+(int)(istart + i*idxi)]);
                       gl.glActiveTexture(GL.GL_TEXTURE3); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz+Math.min((int)(istart + i*idxi)+1, sy-1)]);

                       gl.glBegin(GL.GL_QUADS);{
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[2][0]);
                               gl.glVertex3f(vexts[0][0],vexts[1][0]+i*inc, vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[2][0]);
                               gl.glVertex3f(vexts[0][1],vexts[1][0]+i*inc, vexts[2][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[2][1]);
                               gl.glVertex3f(vexts[0][1],vexts[1][0]+i*inc, vexts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[2][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[2][1]);
                               gl.glVertex3f(vexts[0][0],vexts[1][0]+i*inc, vexts[2][1]);
                       } gl.glEnd();

               }
       }

       private void renZNeg(GLAutoDrawable drawable)
       {
       GL gl = drawable.getGL();
               float dist = vexts[2][1] - vexts[2][0];

               int tsamps = (int)(gluvv.volren.sampleRate * 100 * gluvv.mv.zfSize);
               int samps = (int)(tsamps * (texts[2][1] - texts[2][0]));

               //float inc = gluvv.mv.zfSize/(float)samps;
               float inc = dist/(float)samps;

               float idxi = (float)((1.0/(float)samps) * sz * (dist/gluvv.mv.zfSize));
               int   istart = (int)(texts[2][0] * sz);
               float[] w = new float[4];


               for(int i=samps-1; i>=0; --i){
                       float ds = (float)(1.0f - ((istart + i*idxi) - (Math.floor((istart + i*idxi)))));
                       w[0] = ds; w[1] = ds; w[2] = ds; w[3] = ds;
                       interpSlice(drawable, temptexNames[0],sx,sy,
                                   texNames2D[(int)(istart + i*idxi)],
                                   texNames2D[Math.min((int)(istart + i*idxi)+1, sz-1)], w);

                       w[3] = .3f;
                       gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w, 0); //set w

                       gl.glActiveTexture(GL.GL_TEXTURE0); gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[0]);
                       gl.glActiveTexture(GL.GL_TEXTURE2); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[(int)(istart + i*idxi)]);
                       gl.glActiveTexture(GL.GL_TEXTURE3); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[Math.min((int)(istart + i*idxi)+1, sz-1)]);

                       gl.glBegin(GL.GL_QUADS);{

                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[1][0]);
                               gl.glVertex3f(vexts[0][0],vexts[1][0],vexts[2][0] + i*inc);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[1][0]);
                               gl.glVertex3f(vexts[0][1],vexts[1][0],vexts[2][0] + i*inc);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[1][1]);
                               gl.glVertex3f(vexts[0][1],vexts[1][1],vexts[2][0] + i*inc);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[1][1]);
                               gl.glVertex3f(vexts[0][0],vexts[1][1],vexts[2][0] + i*inc);
                       } gl.glEnd();

               }

       }

       private void renZPos(GLAutoDrawable drawable)
       {
         GL gl = drawable.getGL();
               float dist = vexts[2][1] - vexts[2][0];

               int tsamps = (int)(gluvv.volren.sampleRate * 100 * gluvv.mv.zfSize);
               int samps = (int)(tsamps * (texts[2][1] - texts[2][0]));

               //float inc = gluvv.mv.zfSize/(float)samps;
               float inc = dist/(float)samps;

               float idxi = (1.0f/(float)samps) * sz * (dist/gluvv.mv.zfSize);
               int   istart = (int)(texts[2][0] * sz);
               float[] w = new float[4];


               for(int i=1; i<samps-1; ++i){
                       float ds = (float)(1.0f - ((istart + i*idxi) - (Math.floor((istart + i*idxi)))));
                       w[0] = ds; w[1] = ds; w[2] = ds; w[3] = ds;
                       interpSlice(drawable, temptexNames[0],sx,sy,
                                         texNames2D[(int)(istart + i*idxi)],
                                         texNames2D[Math.min((int)(istart + i*idxi)+1, sz-1)], w);
                       w[3] = .3f;
                       gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w, 0); //set w

                       gl.glActiveTexture(GL.GL_TEXTURE0); gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[0]);
                       gl.glActiveTexture(GL.GL_TEXTURE2); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[(int)(istart + i*idxi)]);
                       gl.glActiveTexture(GL.GL_TEXTURE3); gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[Math.min((int)(istart + i*idxi)+1, sz-1)]);


                       gl.glBegin(GL.GL_QUADS);{
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[1][0]);
                               gl.glVertex3f(vexts[0][0],vexts[1][0],vexts[2][0] + i*inc);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[1][0]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[1][0]);
                               gl.glVertex3f(vexts[0][1],vexts[1][0],vexts[2][0] + i*inc);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][1],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][1],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][1],texts[1][1]);
                               gl.glVertex3f(vexts[0][1],vexts[1][1],vexts[2][0] + i*inc);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE0,texts[0][0],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE2,texts[0][0],texts[1][1]);
                               gl.glMultiTexCoord2f(GL.GL_TEXTURE3,texts[0][0],texts[1][1]);
                               gl.glVertex3f(vexts[0][0],vexts[1][1],vexts[2][0] + i*inc);
                       } gl.glEnd();

               }

     }


     //======================================================== setup shading
//======================================================================

     private void setupShading(GLAutoDrawable drawable)
     {
       GL gl = drawable.getGL();
             if((gluvv.shade == gluvvShade.gluvvShadeDiff) || (gluvv.shade == gluvvShade.gluvvShadeDSpec)){

                     float[] vdir = new float[3];
                     math.subV3(vdir, gluvv.env.eye, gluvv.env.at);
                     math.normalizeV3(vdir);
                     float[] ltdir = new float[3];
                     math.subV3(ltdir, gluvv.light.pos, gluvv.env.at);
                     math.normalizeV3(ltdir);
                     float[] ltoe = new float[3];
                     math.subV3(ltoe,vdir,ltdir);
                     math.scaleV3(.5f, ltoe);
                     float[] half = new float[4];
                     math.addV3(half,ltdir, ltoe);
                     half[3] = 1;
                     float[] vhalf = new float[4];
                     float[] invx = new float[16];
                     math.inverseMatrix(invx, gluvv.rinfo.xform);
                     math.translateV3(vhalf,invx,half);
                     math.negateV3(vhalf);
                     math.normalizeV3(vhalf);
                     vhalf[0] = vhalf[0]/2 + .5f;
                     vhalf[1] = vhalf[1]/2 + .5f;
                     vhalf[2] = vhalf[2]/2 + .5f;
                     gl.glFogfv(GL.GL_FOG_COLOR, vhalf, 0);

                     float[] lpos = new float[4];
                     math.translateV3W(lpos, invx, ltdir);
                     math.negateV3(lpos);
                     math.normalizeV3(ltdir);
                     lpos[0] = lpos[0]/2 + .5f;
                     lpos[1] = lpos[1]/2 + .5f;
                     lpos[2] = lpos[2]/2 + .5f;
                     lpos[3] = gluvv.tf.slider1lo;
                     gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, lpos, 0); //set light direction

                     gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

                     gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 8);

                     //---------------- set up alpha part ------------------
                     if(gluvv.mv1 != null ){

                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                                     GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE0_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable, "nv20volren", " gcom0.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA,
                                     GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE1_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom1.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA,
                                     GL.GL_SPARE0_NV, GL.GL_SPARE1_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom2.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA,
                                     GL.GL_SPARE1_NV, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom3.4a");


                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA,
                                     GL.GL_TEXTURE1, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom4.4a");

                     }

                     //------------------------- set up RGB part -------------------------------------------------
                     //compute tex1 * w + tex2 * (1-w), ie interp shade textures
                     gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_RGB,
                             GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                     GlErr(drawable,"nv20volren", " gcom0.4");

                     //do dot products for 2-sided lighting
                     gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_EXPAND_NEGATE_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_RGB,
                             GL.GL_SPARE1_NV, GL.GL_SPARE0_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, true, true, false);
                     GlErr(drawable,"nv20volren", " gcom1.4");

                     //compute diffuse illumination times color
                     gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER2_NV, GL.GL_RGB,
                             GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE1_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                     GlErr(drawable,"nv20volren", " gcom2.4");

                     //compute specular illumination & ambiant contribution
                     gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_FOG, GL.GL_EXPAND_NORMAL_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                     gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER3_NV, GL.GL_RGB,
                             GL.GL_TEXTURE2, GL.GL_TEXTURE3, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, true, false, false);
                     GlErr(drawable,"nv20volren", " gcom3.4");

                     //diffuse + ambiant
                     gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER4_NV, GL.GL_RGB,
                             GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_TEXTURE3, GL.GL_NONE, GL.GL_NONE, false, false, false);
                     GlErr(drawable,"nv20volren", " gcom4.4");

                     gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER5_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER5_NV, GL.GL_RGB,
                             GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                     GlErr(drawable,"nv20volren", " gcom5.4");

                     gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER6_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER6_NV, GL.GL_RGB,
                             GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                     GlErr(drawable,"nv20volren", " gcom6.4");

                     gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerInputNV(GL.GL_COMBINER7_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glCombinerOutputNV(GL.GL_COMBINER7_NV, GL.GL_RGB,
                             GL.GL_TEXTURE2, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                     GlErr(drawable,"nv20volren", " gcom7.4");


                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_E_TIMES_F_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_E_TIMES_F_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE3, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_TEXTURE2, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                     gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                     GlErr(drawable,"nv20volren", " fcom.0");
             }
             else { //no shading!

                     if(gluvv.mv1 != null ){
                             gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

                             float[] emph = new float[4];
                             emph[3] = gluvv.tf.slider1lo;
                             gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, emph, 0);

                             gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 5);

                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_PRIMARY_COLOR_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                                     GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE0_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom0.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER1_NV, GL.GL_ALPHA,
                                     GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE1_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom1.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER2_NV, GL.GL_ALPHA,
                                     GL.GL_SPARE0_NV, GL.GL_SPARE1_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom2.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER3_NV, GL.GL_ALPHA,
                                     GL.GL_SPARE0_NV, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom3.4a");

                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_SPARE0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerInputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             gl.glCombinerOutputNV(GL.GL_COMBINER4_NV, GL.GL_ALPHA,
                                     GL.GL_TEXTURE1, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
                             GlErr(drawable,"nv20volren", " gcom4.4a");

                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_E_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_F_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
                             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
                             GlErr(drawable,"nv20volren", " fcom.0");
                     }


             }
     }

     //============================================================ draw Clip
//======================================================================
     private void drawClip(GLAutoDrawable drawable, float dv)
     {
       GL gl = drawable.getGL();
             if((gluvv.clip.on == 0)||(gluvv.clip.ortho == 0)) return;
             float[][] c = new float[4][];
             c[0] = gluvv.clip.corners[0];
             c[1] = gluvv.clip.corners[1];
             c[2] = gluvv.clip.corners[2];
             c[3] = gluvv.clip.corners[3];
             c[0][0] = (float)math.CLAMP_ARB(0,c[0][0],gluvv.mv.xfSize);
             c[1][0] = (float)math.CLAMP_ARB(0,c[1][0],gluvv.mv.xfSize);
             c[2][0] = (float)math.CLAMP_ARB(0,c[2][0],gluvv.mv.xfSize);
             c[3][0] = (float)math.CLAMP_ARB(0,c[3][0],gluvv.mv.xfSize);
             c[0][1] = (float)math.CLAMP_ARB(0,c[0][1],gluvv.mv.yfSize);
             c[1][1] = (float)math.CLAMP_ARB(0,c[1][1],gluvv.mv.yfSize);
             c[2][1] = (float)math.CLAMP_ARB(0,c[2][1],gluvv.mv.yfSize);
             c[3][1] = (float)math.CLAMP_ARB(0,c[3][1],gluvv.mv.yfSize);
             c[0][2] = (float)math.CLAMP_ARB(0,c[0][2],gluvv.mv.zfSize);
             c[1][2] = (float)math.CLAMP_ARB(0,c[1][2],gluvv.mv.zfSize);
             c[2][2] = (float)math.CLAMP_ARB(0,c[2][2],gluvv.mv.zfSize);
             c[3][2] = (float)math.CLAMP_ARB(0,c[3][2],gluvv.mv.zfSize);

             gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);
             gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 1);

             // I am just using these to replace the texture's alpha with a different one
             float[] alpha = {1,1,1,gluvv.clip.alpha};
             gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, alpha, 0); //set W

             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom1");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom2");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom3");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom0");

             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);


             if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXPos ) {
                     if((dv < 0)&&(c[0][0] > 0)&&(c[0][0]<gluvv.mv.xfSize)){
                             float[] xw = new float[4];
                             xw[0] = xw[1] = xw[2] = xw[3] =
                                     1-(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx -
                                     (int)(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx));
                             interpSlice(drawable, temptexNames[2],sy,sz,
                                     texNames2D[sz+sy+(int)(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx)],
                                     texNames2D[sz+sy+Math.min((int)(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx)+1, sx-1)],
                                     xw);
                             gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                                     gl.glEnable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[2]);
                             }

                             gl.glColor4f(1,1,1,gluvv.clip.alpha);
                             gl.glBegin(GL.GL_QUADS);{
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[0][1]/gluvv.mv.yfSize,c[0][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[1][1]/gluvv.mv.yfSize,c[1][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[2][1]/gluvv.mv.yfSize,c[2][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[3][1]/gluvv.mv.yfSize,c[3][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
                             } gl.glEnd();
                     }
             } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisXNeg ) {
                     if(dv > 0){
                             float[] xw = new float[4];
                             xw[0] = xw[1] = xw[2] = xw[3] =
                                     1-(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx -
                                     (int)(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx));
                             interpSlice(drawable, temptexNames[2],sy,sz,
                                     texNames2D[sz+sy+(int)(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx)],
                                     texNames2D[sz+sy+Math.min((int)(gluvv.clip.vpos[0]/gluvv.mv.xfSize*sx)+1, sx-1)],
                                     xw);
                             gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                                     gl.glEnable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[2]);
                             }

                             gl.glColor4f(1,1,1,gluvv.clip.alpha);
                             gl.glBegin(GL.GL_QUADS);{
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[0][1]/gluvv.mv.yfSize,c[0][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[1][1]/gluvv.mv.yfSize,c[1][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[2][1]/gluvv.mv.yfSize,c[2][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[3][1]/gluvv.mv.yfSize,c[3][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
                             } gl.glEnd();
                     }
             } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYPos ) {
                     if((dv > 0)&&(c[0][1] > 0)&&(c[0][1]<gluvv.mv.yfSize)){
                             float[] yw = new float[4];
                             yw[0] = yw[1] = yw[2] = yw[3] =
                                     1-(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy -
                                     (int)(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy));
                             interpSlice(drawable, temptexNames[1],sx,sz,
                                     texNames2D[sz+(int)(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy)],
                                     texNames2D[sz+Math.min((int)(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy)+1, sy-1)],
                                     yw);
                             gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                                     gl.glEnable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[1]);
                             }

                             gl.glColor4f(1,1,1,gluvv.clip.alpha);
                             gl.glBegin(GL.GL_QUADS);{
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[0][0]/gluvv.mv.xfSize,c[0][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[1][0]/gluvv.mv.xfSize,c[1][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[2][0]/gluvv.mv.xfSize,c[2][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[3][0]/gluvv.mv.xfSize,c[3][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
                             } gl.glEnd();
                     }
             } else if (  gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisYNeg ) {
                     if((dv < 0)&&(c[0][1] > 0)&&(c[0][1]<gluvv.mv.yfSize)){
                             float[] yw = new float[4];
                             yw[0] = yw[1] = yw[2] = yw[3] =
                                     1-(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy -
                                     (int)(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy));
                             interpSlice(drawable, temptexNames[1],sx,sz,
                                     texNames2D[sz+(int)(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy)],
                                     texNames2D[sz+Math.min((int)(gluvv.clip.vpos[1]/gluvv.mv.yfSize*sy)+1, sy-1)],
                                     yw);
                             gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                                     gl.glEnable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[1]);
                             }

                             gl.glColor4f(1,1,1,gluvv.clip.alpha);
                             gl.glBegin(GL.GL_QUADS);{
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[0][0]/gluvv.mv.xfSize,c[0][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[1][0]/gluvv.mv.xfSize,c[1][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[2][0]/gluvv.mv.xfSize,c[2][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[3][0]/gluvv.mv.xfSize,c[3][2]/gluvv.mv.zfSize);
                                     gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
                             } gl.glEnd();
                     }
             } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZPos ) {
                     if((dv < 0)&&(c[0][2] > 0)&&(c[0][2]<gluvv.mv.zfSize)){
                             float[] zw = new float[4];
                             zw[0] = zw[1] = zw[2] = zw[3] =
                                     1-(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz -
                                     (int)(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz));
                             interpSlice(drawable, temptexNames[0],sx,sz,
                                     texNames2D[(int)(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz)],
                                     texNames2D[Math.min((int)(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz)+1, sz-1)],
                                     zw);
                             gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                                     gl.glEnable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[0]);
                             }

                             gl.glColor4f(1,1,1,gluvv.clip.alpha);
                             gl.glBegin(GL.GL_QUADS);{
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[0][0]/gluvv.mv.xfSize,c[0][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[1][0]/gluvv.mv.xfSize,c[1][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[2][0]/gluvv.mv.xfSize,c[2][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[3][0]/gluvv.mv.xfSize,c[3][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
                             } gl.glEnd();
                     }
             } else if ( gluvv.clip.oaxis == VolRenMajorAxis.VolRenAxisZNeg ) {
                     if((dv > 0)&&(c[0][2] > 0)&&(c[0][2]<gluvv.mv.zfSize)){
                             float[] zw = new float[4];
                             zw[0] = zw[1] = zw[2] = zw[3] =
                                     1-(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz -
                                     (int)(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz));
                             interpSlice(drawable, temptexNames[0],sx,sz,
                                     texNames2D[(int)(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz)],
                                     texNames2D[Math.min((int)(gluvv.clip.vpos[2]/gluvv.mv.zfSize*sz)+1, sz-1)],
                                     zw);
                             gl.glActiveTexture(GL.GL_TEXTURE0); { //this is for the slice
                                     gl.glEnable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[0]);
                             }

                             gl.glColor4f(1,1,1,gluvv.clip.alpha);
                             gl.glBegin(GL.GL_QUADS);{
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[0][0]/gluvv.mv.xfSize,c[0][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[0][0], c[0][1], c[0][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[1][0]/gluvv.mv.xfSize,c[1][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[1][0], c[1][1], c[1][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[2][0]/gluvv.mv.xfSize,c[2][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[2][0], c[2][1], c[2][2]);
                                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,c[3][0]/gluvv.mv.xfSize,c[3][1]/gluvv.mv.yfSize);
                                     gl.glVertex3f(c[3][0], c[3][1], c[3][2]);
                             } gl.glEnd();
                     }

             }

             gl.glDisable(GL.GL_REGISTER_COMBINERS_NV);  //clean up opengl state
             gl.glActiveTexture(GL.GL_TEXTURE0); {
                                     gl.glDisable(GL.GL_TEXTURE_2D);
                                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
                                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
             }

     }


     //======================================================== Interp Slices
//======================================================================

     private void interpSlices(GLAutoDrawable drawable)
     {
       GL gl = drawable.getGL();

             gl.glPushMatrix();{
                     gl.glTranslatef(gluvv.rinfo.trans[0], //translate
                                                                      gluvv.rinfo.trans[1],
                                                                      gluvv.rinfo.trans[2]);
                     gl.glMultMatrixf(gluvv.rinfo.xform, 0);  //rotate
                     gl.glTranslatef(-gluvv.mv.xfSize/2,  //center
                                                                      -gluvv.mv.yfSize/2,
                                                                      -gluvv.mv.zfSize/2);

             float[] lweight = {.5f, .5f,.5f,.5f};

             gl.glClear(GL.GL_COLOR_BUFFER_BIT);

             gl.glActiveTexture(GL.GL_TEXTURE0); {
                     gl.glEnable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                     //gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_BLEND);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz/3]);
             }

             gl.glActiveTexture(GL.GL_TEXTURE1); {
                     gl.glEnable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                     //gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_BLEND);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz/3 * 2]);
             }

             gl.glActiveTexture(GL.GL_TEXTURE2); {
                     gl.glDisable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
             }
             gl.glActiveTexture(GL.GL_TEXTURE3); {
                     gl.glDisable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
             }

             float[] rweight = {1.0f-.5f, 0f,0f,0f};

             gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, lweight, 0);
             gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR1_NV, rweight, 0);

             gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

             //gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 0);
             GlErr(drawable,"nv20volren", " fcom4");
              //gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             //gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             //gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
             //gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             //gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE1_NV, GL.GL_NONE, GL.GL_NONE, GL.GL_FALSE, GL.GL_FALSE, GL.GL_FALSE);
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom1");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom2");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom3");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom0");

             gl.glBegin(GL.GL_QUADS);{
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,0,0);
                     //gl.glTexCoord2f(0.0,0.0);
                     gl.glVertex3f(0,0,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,1,0);
                     //gl.glTexCoord2f(1.0,0.0);
                     gl.glVertex3f(1f,0,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,1,1);
                     //gl.glTexCoord2f(1.0,1.0);
                     gl.glVertex3f(1f,1f,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,0,1);
                     //gl.glTexCoord2f(0.0,1.0);
                     gl.glVertex3f(0,1f,.5f);
             } gl.glEnd();

             gl.glDisable(GL.GL_REGISTER_COMBINERS_NV);

             gl.glActiveTexture(GL.GL_TEXTURE1); {
                     gl.glDisable(GL.GL_TEXTURE_2D);
             }
             gl.glActiveTexture(GL.GL_TEXTURE0); {
                     gl.glEnable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz/3]);
             }
             gl.glBegin(GL.GL_QUADS);{
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                     //gl.glTexCoord2f(0.0,0.0);
                     gl.glVertex3f(-1f,0,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                     //gl.glTexCoord2f(1.0,0.0);
                     gl.glVertex3f(0,0,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                     //gl.glTexCoord2f(1.0,1.0);
                     gl.glVertex3f(0,1f,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                     //gl.glTexCoord2f(0.0,1.0);
                     gl.glVertex3f(-1f,1f,.5f);
             } gl.glEnd();


             lweight[0]=lweight[1]=lweight[2]=lweight[3]=gluvv.volren.interactSamp;

             interpSlice(drawable, temptexNames[0],512,512,texNames2D[sz/3],texNames2D[sz/3*2], lweight);



             gl.glActiveTexture(GL.GL_TEXTURE0); {
                     gl.glEnable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[0]);
             }
             gl.glActiveTexture(GL.GL_TEXTURE1); {
                     gl.glDisable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
             }

             gl.glBegin(GL.GL_QUADS);{
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                     gl.glVertex3f(0,-1f,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                     gl.glVertex3f(1f,-1f,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                     gl.glVertex3f(1f,0,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                     gl.glVertex3f(0,0,.5f);
             } gl.glEnd();



             }gl.glPopMatrix();
     }

     //========================================================== Draw Slices
//======================================================================

     private void drawSlices(GLAutoDrawable drawable)
     {
             GL gl = drawable.getGL();
             gl.glDisable(GL.GL_LIGHTING);

             gl.glPushMatrix();{
                     gl.glTranslatef(gluvv.rinfo.trans[0], //translate
                                                                      gluvv.rinfo.trans[1],
                                                                      gluvv.rinfo.trans[2]);
                     gl.glMultMatrixf(gluvv.rinfo.xform, 0);  //rotate
                     gl.glTranslatef(-gluvv.mv.xfSize/2,  //center
                                                                      -gluvv.mv.yfSize/2,
                                                                      -gluvv.mv.zfSize/2);

                     gl.glEnable(GL.GL_TEXTURE_SHADER_NV);
                     GlErr(drawable,"nv20volren", " enable shader");

                     gl.glActiveTexture(GL.GL_TEXTURE0); {
                             gl.glEnable(GL.GL_TEXTURE_2D);
                             gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
                             gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                             gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz/2]);
                     }
                     gl.glActiveTexture(GL.GL_TEXTURE1); {
                             gl.glEnable(GL.GL_TEXTURE_2D);
                             gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                             gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_DEPENDENT_AR_TEXTURE_2D_NV);
                             gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_PREVIOUS_TEXTURE_INPUT_NV, GL.GL_TEXTURE0);
                             gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
                     }
                     gl.glActiveTexture(GL.GL_TEXTURE2); {
                             gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                             gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
                     }
                     gl.glActiveTexture(GL.GL_TEXTURE3); {
                             gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
                             gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_NONE);
                     }

                     gl.glBegin(GL.GL_QUADS);{
                             gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                             //gl.glTexCoord2f(0.0,0.0);
                             gl.glVertex3f(0,0,.5f);

                             gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                             //gl.glTexCoord2f(1.0,0.0);
                             gl.glVertex3f(1f,0,.5f);

                             gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                             //gl.glTexCoord2f(1.0,1.0);
                             gl.glVertex3f(1f,1f,.5f);

                             gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                             //gl.glTexCoord2f(0.0,1.0);
                             gl.glVertex3f(0,1f,.5f);
                     } gl.glEnd();

                     gl.glDisable(GL.GL_TEXTURE_SHADER_NV);
                     gl.glActiveTexture(GL.GL_TEXTURE1);{
                             gl.glDisable(GL.GL_TEXTURE_2D);
                     }
                     gl.glActiveTexture(GL.GL_TEXTURE0);{
                             gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                     }

                     //gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz + (sy/2)]);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);
                     gl.glBegin(GL.GL_QUADS);{
                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                             gl.glTexCoord2f(0.0f,0.0f);
                             gl.glVertex3f(0,.5f,0);

                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                             gl.glTexCoord2f(1.0f,0.0f);
                             gl.glVertex3f(1f,.5f,0);

                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                             gl.glTexCoord2f(1.0f,1.0f);
                             gl.glVertex3f(1f,.5f,1f);

                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                             gl.glTexCoord2f(0.0f,1.0f);
                             gl.glVertex3f(0,.5f, 1);
                     } gl.glEnd();

                     gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz + sy + (sx/2)]);

                     gl.glBegin(GL.GL_QUADS);{
                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                             gl.glTexCoord2f(0.0f,0.0f);
                             gl.glVertex3f(.5f,0,0);

                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                             gl.glTexCoord2f(1.0f,0.0f);
                             gl.glVertex3f(.5f,1,0);

                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                             gl.glTexCoord2f(1.0f,1.0f);
                             gl.glVertex3f(.5f,1,1);

                             //gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                             gl.glTexCoord2f(0.0f,1.0f);
                             gl.glVertex3f(.5f,0, 1);
                     } gl.glEnd();




             } gl.glPopMatrix();

             gl.glDisable(GL.GL_TEXTURE_2D);
             gl.glEnable(GL.GL_LIGHTING);

     }

//=================================================== Create 2D Textures
//======================================================================

     private void create2DTextures(GLAutoDrawable drawable, MetaVolume mv) {
       GL gl = drawable.getGL();
       /*
// #if 0
       byte[] data = mv.volumes[0].currentData;

       sx = mv.xiSize; //sizes
       sy = mv.yiSize;
       sz = mv.ziSize;

       byte[] gmag;
       byte[] gmag2 = new byte[sx * sy * sz];
       byte[] grad = new byte[sx * sy * sz * 3];
       float[] fgrad = new float[sx * sy * sz * 3];

       if (gluvv.mv1 == null) { //no second axis specified, use gmag
         System.err.println(" Computing derivatives ...");
         gmag = new byte[sx * sy * sz];
         math.derivative3D(gmag, fgrad, sx, sy, sz, data);
         // gluvv.mv.volumes[0].currentData1 = gmag;                  // ?????????????????????????????????????????????????
       }
       else { //second axis IS specified, compute a correlation derivative & gmag2
         gmag = gluvv.mv1.volumes[0].currentData;
         // gluvv.mv.volumes[0].currentData1 = gmag;                   // ????????????????????????????????????????????
         math.addDer(gmag2, fgrad, sx, sy, sz, data, gmag);
       }

       System.err.print("  Blurring normals");
       math.blurV3D(fgrad, 4, 1, 1, 1, sx, sy, sz);
       System.err.println("...");
       //blurV3DN(fgrad, 10, 1, 1, 1, sx, sy, sz);
       System.err.println("  Scale & Bias");

       math.scalebiasN(grad, fgrad, sx, sy, sz);

       System.err.println("creating textures " + sz + " x " + sy + " x " + sx);

       int stx = 1; //strides
       int sty = sx;
       int stz = sx * sy;

       if (texNames2D != null) texNames2D = null;
       texNames2D = new int[sz + sy + sx];
       // gl.glGenTextures(sz + sy + sx, texNames2D);                      // ?????????????????????????????????????????????????
       GlErr(drawable,"NV20VolRen", " genTextures - data textues");
       //if(shadeNames) delete[] shadeNames;
       shadeNames = new int[sz + sy + sx];
       // gl.glGenTextures(sz + sy + sx, shadeNames);                      // ??????????????????????????????????????????????????
       GlErr(drawable,"NV20VolRen", " genTextures - shade textures");

       gl.glEnable(GL.GL_TEXTURE_2D);

       // create z axis textures
       byte[] tex = new byte[sx * sy * 4]; //temperary z axis texture
       byte[] shade = new byte[sx * sy * 3]; //temp shade texture

       System.err.println("Creating Axies: Z " + sz);

       for (int i = 0; i < sz; ++i) {
         for (int j = 0; j < sy; ++j) {
           for (int k = 0; k < sx; ++k) {
             tex[j * sx * 4 + k * 4 + 0] = 0; //data[i*stz + j*sty + k];
             tex[j * sx * 4 + k * 4 + 1] = data[i * stz + j * sty + k];
             tex[j * sx * 4 + k * 4 + 2] = gmag[i * stz + j * sty + k]; //0;//data[i*stz + j*sty + k];
             tex[j * sx * 4 + k * 4 + 3] = gmag2[i * stz + j * sty + k]; //data[i*stz + j*sty + k];
             shade[j * sx * 3 + k * 3 + 0] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 0];
             shade[j * sx * 3 + k * 3 + 1] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 1];
             shade[j * sx * 3 + k * 3 + 2] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 2];
           }
         }
         //gl.glEnable(GL.GL_TEXTURE_2D);
         gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[i]);

         gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                            GL.GL_LINEAR);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                            GL.GL_LINEAR);

         ByteBuffer texBuffer = ByteBuffer.wrap(tex);
         texBuffer.rewind();

         gl.glTexImage2D(GL.GL_TEXTURE_2D,
                         0,
                         GL.GL_RGBA8,
                         sx,
                         sy,
                         0,
                         GL.GL_RGBA,
                         GL.GL_UNSIGNED_BYTE,
                         texBuffer);

         gl.glFlush();

         gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[i]);

         gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                            GL.GL_LINEAR);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                            GL.GL_LINEAR);

         ByteBuffer shadeBuffer = ByteBuffer.wrap(shade);
         shadeBuffer.rewind();
         gl.glTexImage2D(GL.GL_TEXTURE_2D,
                         0,
                         GL.GL_RGB8,
                         sx,
                         sy,
                         0,
                         GL.GL_RGB,
                         GL.GL_UNSIGNED_BYTE,
                         shadeBuffer);
         gl.glFlush();
       }
// #if 1
       // create y axis textures
       tex = null;
       shade = null;
 */
       byte[] texY = new byte[sx * sz * 4]; //temperary z axis texture
       byte[] shadeY = new byte[sx * sz * 3]; //temperary z axis texture

       byte[] data = mv.volumes[0].currentData;

        sx = mv.xiSize; //sizes
        sy = mv.yiSize;
        sz = mv.ziSize;

        byte[] gmag;
        byte[] gmag2 = new byte[sx * sy * sz];
        byte[] grad = new byte[sx * sy * sz * 3];
        float[] fgrad = new float[sx * sy * sz * 3];

        int stx = 1; //strides
        int sty = sx;
        int stz = sx * sy;

        if (gluvv.mv1 == null) { //no second axis specified, use gmag
          System.err.println(" Computing derivatives ...");
          gmag = new byte[sx * sy * sz];
          math.derivative3D(gmag, fgrad, sx, sy, sz, data);
          // gluvv.mv.volumes[0].currentData1 = gmag;                  // ?????????????????????????????????????????????????
        }
        else { //second axis IS specified, compute a correlation derivative & gmag2
          gmag = gluvv.mv1.volumes[0].currentData;
          // gluvv.mv.volumes[0].currentData1 = gmag;                   // ????????????????????????????????????????????
          math.addDer(gmag2, fgrad, sx, sy, sz, data, gmag);
       }

       System.err.print(" Y " + sy);

       for (int j = 0; j < sy; ++j) {
         for (int i = 0; i < sz; ++i) {
           for (int k = 0; k < sx; ++k) {
             texY[i * sx * 4 + k * 4 + 0] = 0; //data[i*stz + j*sty + k];
             texY[i * sx * 4 + k * 4 + 1] = data[i * stz + j * sty + k]; //0;
             texY[i * sx * 4 + k * 4 + 2] = gmag[i * stz + j * sty + k]; //0;
             texY[i * sx * 4 + k * 4 + 3] = gmag2[i * stz + j * sty + k];
             shadeY[i * sx * 3 + k * 3 + 0] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 0];
             shadeY[i * sx * 3 + k * 3 + 1] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 1];
             shadeY[i * sx * 3 + k * 3 + 2] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 2];
           }
         }
         gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz + j]);
         gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                            GL.GL_LINEAR);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                            GL.GL_LINEAR);
         ByteBuffer texYBuffer = ByteBuffer.wrap(texY);
         texYBuffer.rewind();
         gl.glTexImage2D(GL.GL_TEXTURE_2D,
                         0,
                         GL.GL_RGBA8,
                         sx,
                         sz,
                         0,
                         GL.GL_RGBA,
                         GL.GL_UNSIGNED_BYTE,
                         texYBuffer);
         gl.glFlush();

         gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz + j]);
         gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                            GL.GL_LINEAR);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                            GL.GL_LINEAR);
         ByteBuffer shadeYBuffer = ByteBuffer.wrap(shadeY);
         shadeYBuffer.rewind();
         gl.glTexImage2D(GL.GL_TEXTURE_2D,
                         0,
                         GL.GL_RGB8,
                         sx,
                         sz,
                         0,
                         GL.GL_RGB,
                         GL.GL_UNSIGNED_BYTE,
                         shadeYBuffer);

         gl.glFlush();
       }

       // create x axis textures
       texY = null;
       shadeY = null;

       byte[] texX = new byte[sy * sz * 4]; //temperary z axis texture
       byte[] shadeX = new byte[sy * sz * 3]; //temperary z axis texture

       System.err.print(" X " + sx);

       for (int k = 0; k < sx; ++k) {
         for (int i = 0; i < sz; ++i) {
           for (int j = 0; j < sy; ++j) {
             texX[i * sy * 4 + j * 4 + 0] = 0; //data[i*stz + j*sty + k];
             texX[i * sy * 4 + j * 4 + 1] = data[i * stz + j * sty + k]; //0;
             texX[i * sy * 4 + j * 4 + 2] = gmag[i * stz + j * sty + k]; //0;
             texX[i * sy * 4 + j * 4 + 3] = gmag2[i * stz + j * sty + k];
             shadeX[i * sy * 3 + j * 3 + 0] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 0];
             shadeX[i * sy * 3 + j * 3 + 1] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 1];
             shadeX[i * sy * 3 + j * 3 + 2] = grad[i * stz * 3 + j * sty * 3 +
                 k * 3 + 2];
           }
         }
         gl.glBindTexture(GL.GL_TEXTURE_2D, texNames2D[sz + sy + k]);

         gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                            GL.GL_LINEAR);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                            GL.GL_LINEAR);
         ByteBuffer texXBuffer = ByteBuffer.wrap(texX);
         texXBuffer.rewind();
         gl.glTexImage2D(GL.GL_TEXTURE_2D,
                         0,
                         GL.GL_RGBA8,
                         sy,
                         sz,
                         0,
                         GL.GL_RGBA,
                         GL.GL_UNSIGNED_BYTE,
                         texXBuffer);
         gl.glFlush();

         gl.glBindTexture(GL.GL_TEXTURE_2D, shadeNames[sz + sy + k]);
         gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER,
                            GL.GL_LINEAR);
         gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER,
                            GL.GL_LINEAR);
         ByteBuffer shadeXBuffer = ByteBuffer.wrap(shadeX);
         shadeXBuffer.rewind();

         gl.glTexImage2D(GL.GL_TEXTURE_2D,
                         0,
                         GL.GL_RGB8,
                         sy,
                         sz,
                         0,
                         GL.GL_RGB,
                         GL.GL_UNSIGNED_BYTE,
                         shadeXBuffer);
         gl.glFlush();

       }




               gl.glDisable(GL.GL_TEXTURE_2D);

               GlErr(drawable,"NV20VolRen", " gl.glTexImage2D");

     }

//==================================================== Create 2D Dep Tex
//======================================================================

     private void create2DDepTex(GLAutoDrawable drawable)
     {
           GL gl = drawable.getGL();
             gl.glGenTextures(1, deptexName, 0);
             gl.glGenTextures(1, gluvv.volren.deptexName, 0);

             deptex = new byte[256*256*4]; //reference texture

             gluvv.volren.deptex = deptex;

             for(int j=0; j<256; ++j){
                     for(int k=0; k<256; ++k){
                             deptex[j*256*4 + k*4 + 0] =   new Float(k).byteValue();
                             deptex[j*256*4 + k*4 + 1] = 	new Float(j).byteValue();//k==0 ? 0 : 255;
                             deptex[j*256*4 + k*4 + 2] = 	new Float(255 - j).byteValue(); //k==0 ? 255 : 0;
                             deptex[j*256*4 + k*4 + 3] = 	new Float((j/(float)2)).byteValue();
                     }
             }

             gDeptex = new byte[256*256*4]; //for good sample rates
             iDeptex = new byte[256*256*4]; //for interactive sample rate
             copyScale(gluvv.volren.goodSamp, gDeptex);
             copyScale(gluvv.volren.interactSamp, iDeptex);

             lastSamp = gluvv.volren.goodSamp;
             lastGoodSamp = gluvv.volren.goodSamp;
             lastInteSamp = gluvv.volren.interactSamp;

             loadDepTex(drawable, gDeptex);

             gl.glEnable(GL.GL_TEXTURE_2D);
             gl.glBindTexture(GL.GL_TEXTURE_2D, gluvv.volren.deptexName[0]);

             gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
             gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
        ByteBuffer deptexBuffer = ByteBuffer.wrap(deptex);
       deptexBuffer.rewind();
             gl.glTexImage2D(GL.GL_TEXTURE_2D,
                             0,
                             GL.GL_RGBA8,
                             256,
                             256,
                             0,
                             GL.GL_RGBA,
                             GL.GL_UNSIGNED_BYTE,
                             deptexBuffer);
             gl.glFlush();

     }



     private void loadDepTex(GLAutoDrawable drawable, byte[] dtex)
     {
       GL gl = drawable.getGL();
             gl.glEnable(GL.GL_TEXTURE_2D);
             gl.glBindTexture(GL.GL_TEXTURE_2D, deptexName[0]);

             gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
             gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
        ByteBuffer dtexBuffer = ByteBuffer.wrap(dtex);
        dtexBuffer.rewind();
             gl.glTexImage2D(GL.GL_TEXTURE_2D,
                             0,
                             GL.GL_RGBA8,
                             256,
                             256,
                             0,
                             GL.GL_RGBA,
                             GL.GL_UNSIGNED_BYTE,
                             dtexBuffer);
             gl.glFlush();
     }

//=================================================== copy & scale alpha
//======================================================================

     private void scaleAlpha(float nsr, float osr, byte[] dtex)
     {
             if(nsr == osr) return;
             float alphaScale = 1/nsr;
             for(int i=0; i<256; ++i){
                     for(int j=0; j<256; ++j){
                             dtex[i*256*4 + j*4 + 3] =
                                  new Float((1f- Math.pow((1.0f- deptex[i*256*4 + j*4 + 3]/255.0f), alphaScale))*255f).byteValue();
                     }
             }
     }

     private void copyScale(float sr, byte[] dtex)
     {
             float alphaScale = 1.0f/sr;
             for(int i=0; i<256; ++i){
                     for(int j=0; j<256; ++j){
                             dtex[i*256*4 + j*4 + 0] = deptex[i*256*4 + j*4 + 0];
                             dtex[i*256*4 + j*4 + 1] = deptex[i*256*4 + j*4 + 1];
                             dtex[i*256*4 + j*4 + 2] = deptex[i*256*4 + j*4 + 2];
                             dtex[i*256*4 + j*4 + 3] =
                                 new Float((1.0f - Math.pow((1.0f-(deptex[i*256*4 + j*4 + 3]/255.0f)), alphaScale))*255f).byteValue();
                     }
             }
     }


//====================================================== Create Temp Tex
//======================================================================

     private void createTempTex(GLAutoDrawable drawable)
     {
       GL gl = drawable.getGL();
             byte[] texZ = new byte[sx*sy*4];
             byte[] texY = new byte[sx*sz*4];
             byte[] texX = new byte[sy*sz*4];

             gl.glGenTextures(1, temptexNames, 0);

             gl.glEnable(GL.GL_TEXTURE_2D);
             gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[0]);

             gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
             gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

        ByteBuffer texZBuffer = ByteBuffer.wrap(texZ);
        texZBuffer.rewind();

             gl.glTexImage2D(GL.GL_TEXTURE_2D,
                             0,
                             GL.GL_RGBA8,
                             sx,
                             sy,
                             0,
                             GL.GL_RGBA,
                             GL.GL_UNSIGNED_BYTE,
                             texZBuffer);
             gl.glFlush();

             gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[1]);

             gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
             gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

       ByteBuffer texYBuffer = ByteBuffer.wrap(texY);
       texYBuffer.rewind();

             gl.glTexImage2D(GL.GL_TEXTURE_2D,
                             0,
                             GL.GL_RGBA8,
                             sx,
                             sz,
                             0,
                             GL.GL_RGBA,
                             GL.GL_UNSIGNED_BYTE,
                             texYBuffer);
             gl.glFlush();

             gl.glBindTexture(GL.GL_TEXTURE_2D, temptexNames[2]);

             gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
             gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
       gl.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);

          ByteBuffer texXBuffer = ByteBuffer.wrap(texX);
          texXBuffer.rewind();
             gl.glTexImage2D(GL.GL_TEXTURE_2D,
                             0,
                             GL.GL_RGBA8,
                             sy,
                             sz,
                             0,
                             GL.GL_RGBA,
                             GL.GL_UNSIGNED_BYTE,
                             texXBuffer);
             gl.glFlush();
     }

//========================================================= Make Current
//======================================================================

     private void MakeCurrent(GLAutoDrawable drawable)
     {

         //     gl.glutSetWindow(gluvv.mainWindow);          ??????????????????????????????
     }

//========================================================== interpSlice
//======================================================================

     private void interpSlice(GLAutoDrawable drawable, int tname,     //temperary texture to write to
                                            int width, int height,  //size of textures
                                                                                        int atname,    //A texture
                                                                                        int btname,    //B texture
                                                                                        float[] w0)       //w0[4], W, compute: AW + (1-W)B .tname
     {
         GL gl = drawable.getGL();
             //cerr << "w = " << width << " h = " << height << "atname" <<  atname << "  btname" << btname << endl;
             if(gluvv.picking == 1) return;
             //pbuff.HandleModeSwitch();
             // pbuff.MakeCurrent();  ?????????????????????????????????????????????????

       GlErr(drawable,"nv20volren", " is-fcom4.7");
             //gl.glClear( GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT );

             gl.glActiveTexture(GL.GL_TEXTURE0); { //A texture
                     gl.glEnable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, atname);
             }

             gl.glActiveTexture(GL.GL_TEXTURE1); { //B texture
                     gl.glEnable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
                     gl.glBindTexture(GL.GL_TEXTURE_2D, btname);
             }

             gl.glActiveTexture(GL.GL_TEXTURE2); { //nothing
                     gl.glDisable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
             }
             gl.glActiveTexture(GL.GL_TEXTURE3); { //nothing
                     gl.glDisable(GL.GL_TEXTURE_2D);
                     gl.glTexEnvi(GL.GL_TEXTURE_SHADER_NV, GL.GL_SHADER_OPERATION_NV, GL.GL_NONE);
             }

             float[] w1 = {w0[0], w0[1], w0[2], w0[3]};
             float[] w2 = {1-w0[0], 1-w0[1], 1-w0[2], 1-w0[3]};

             GlErr(drawable,"nv20volren", " is-fcom4.6");

             gl.glCombinerParameterfvNV(GL.GL_CONSTANT_COLOR0_NV, w0, 0); //set W
             if ( GlErr(drawable,"nv20volren", " is-fcom4.5") == 1 ) {
               System.err.println(" w" + w0[0] + " " + w0[1] + " " + w0[2] + " " + w0[3]);
             }


             gl.glEnable(GL.GL_REGISTER_COMBINERS_NV);

     /*

             //computes: A(W) + (1-W)B + 0
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             System.err.println("nv20volren","fcom1");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             System.err.println("nv20volren","fcom2");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             System.err.println("nv20volren","fcom3");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             System.err.println("nv20volren","fcom0");

     */

             gl.glCombinerParameteriNV(GL.GL_NUM_GENERAL_COMBINERS_NV, 1);
             GlErr(drawable,"nv20volren", " is-fcom4.4");
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_RGB, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_RGB);
             gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_RGB,
                     GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE1_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
             GlErr(drawable,"nv20volren", " is-fcom4.3");

             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_A_NV, GL.GL_TEXTURE0, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_B_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_C_NV, GL.GL_TEXTURE1, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
             gl.glCombinerInputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA, GL.GL_VARIABLE_D_NV, GL.GL_CONSTANT_COLOR0_NV, GL.GL_UNSIGNED_INVERT_NV, GL.GL_ALPHA);
             gl.glCombinerOutputNV(GL.GL_COMBINER0_NV, GL.GL_ALPHA,
                     GL.GL_DISCARD_NV, GL.GL_DISCARD_NV, GL.GL_SPARE1_NV, GL.GL_NONE, GL.GL_NONE, false, false, false);
             GlErr(drawable,"nv20volren", " is-fcom4.2");

             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_A_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom1");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_B_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom2");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_C_NV, GL.GL_ZERO, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " fcom3");
             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_D_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_RGB);
             GlErr(drawable,"nv20volren", " is-fcom0");

             gl.glFinalCombinerInputNV(GL.GL_VARIABLE_G_NV, GL.GL_SPARE1_NV, GL.GL_UNSIGNED_IDENTITY_NV, GL.GL_ALPHA);
             GlErr(drawable,"nv20volren", " is-fcom4.1");



             gl.glColor4f(1f,1f,1f,1f);
             gl.glBegin(GL.GL_QUADS);{
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,0);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,0,0);
                     gl.glVertex3f(0,0,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,0);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,1,0);
                     gl.glVertex3f(width,0f,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,1,1);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,1,1);
                     gl.glVertex3f(width,height,.5f);

                     gl.glMultiTexCoord2f(GL.GL_TEXTURE0,0,1);
                     gl.glMultiTexCoord2f(GL.GL_TEXTURE1,0,1);
                     gl.glVertex3f(0,height,.5f);
             } gl.glEnd();


             gl.glDisable(GL.GL_REGISTER_COMBINERS_NV);

             gl.glBindTexture(GL.GL_TEXTURE_2D, tname);
             //see if gl.glCopyTexImage2D is faster!
             gl.glCopyTexSubImage2D(GL.GL_TEXTURE_2D, 0, 0, 0, 0, 0, width, height);
             gl.glFlush();

             // MakeCurrent();                  ?????????????????????????????????????????????????????
             GlErr(drawable,"nv20volren", " is-makecurrent");

  }


  //=====================================================================
  private int GlErr(GLAutoDrawable drawable, String location, String glfuncname)
  {
    int errCode;
    String errString;
    GL gl = drawable.getGL();

    if((errCode = gl.glGetError()) != GL.GL_NO_ERROR){
      errString = glu.gluErrorString(errCode);
      System.err.println("OpenGL ERROR : " + location + "::" + glfuncname + " : "
           + errString);
      return 1;
    }
    return 0;
  }

}
