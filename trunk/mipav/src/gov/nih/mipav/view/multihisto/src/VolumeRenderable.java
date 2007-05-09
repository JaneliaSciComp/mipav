package gov.nih.mipav.view.multihisto.src;


import javax.media.opengl.*;

public class VolumeRenderable extends gluvvPrimitive {

  private VolumeRenderer volren;
  private TLUT tlut;
  private gluvvGlobal gluvv;


  public VolumeRenderable(gluvvGlobal _gluvv)
  {
        gluvv = _gluvv;
        volren = null;
  }


  public void draw(GLAutoDrawable drawable)
 {
        if(volren == null) return;
        GL gl = drawable.getGL();
        gl.glPushMatrix();
        {
                gl.glTranslatef(gluvv.rinfo.trans[0], //translate
                                   gluvv.rinfo.trans[1],
                                   gluvv.rinfo.trans[2]);
                gl.glMultMatrixf(gluvv.rinfo.xform, 0);  //rotate
                gl.glTranslatef(-gluvv.mv.xfSize/2,  //center
                                   -gluvv.mv.yfSize/2,
                                   -gluvv.mv.zfSize/2);
                double[] mv = new double[16];
                gl.glGetDoublev(GL.GL_MODELVIEW_MATRIX, mv, 0);
                ((TLUT)(volren.getColorMap())).scaleAlpha(drawable, gluvv.volren.sampleRate);
                if(gluvv.volren.loadTLUT == 1){
                        gluvv.volren.loadTLUT = 0;
                        (volren.getColorMap()).loadTransferTableRGBA(drawable);
                }
                volren.renderVolume(drawable, gluvv.volren.sampleRate, mv);
                System.err.println("VolumeRenderable::" + "draw()");
        }
        gl.glPopMatrix();
  }

  public void init(GLAutoDrawable drawable)
  {
          if(gluvv.mv == null){
                  System.err.println("ERROR: VolumeRenderable::init(), no MetaVolume defined");
                  return;
          }
          volren = new VolumeRenderer(gluvv.mv, 0);
          if(gluvv.mv.numSubVols == 1){
                  volren.createVolume(drawable,VolumeRenderer.VolRen3DExt, gluvv.mv.volumes[0]);
          } else {
                  volren.createVolume(drawable, VolumeRenderer.VolRen3DExt, gluvv.mv.volumes, gluvv.mv.numSubVols);
          }
          volren.createTLUT();
          gluvv.volren.tlut = (volren.getColorMap());
          (volren.getColorMap()).rgbBlackBody();
          (volren.getColorMap()).rgbCyanMagenta();
          (volren.getColorMap()).rgbSpectral();
          (volren.getColorMap()).alphaRamp(0, 255, 0.0f, 0.1f);
          (volren.getColorMap()).loadTransferTableRGBA(drawable);
          //tlut.rgbBlackBody();
          //tlut.alphaRamp(0,255,0,1);
          //tlut.loadTransferTableRGBA();
  }


}
