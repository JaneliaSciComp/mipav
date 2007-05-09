package gov.nih.mipav.view.gpu.src;
import java.awt.Font;


class Fancytext extends Image24
{
 private Fancytext()
 {super(0,0);
 }

 public static Image24 shadowtext(Font f,String s,int textc,int shadowc,int shift)
 {
  Image24 r=rendertext(f,s,0,shift,0,shift);
  r.colorreplace(color(0,0,0,255),textc);
  Image24 shadow=r.makeshadow(shadowc);
  shadow.translate(shift,shift);
  shadow.wind(10,3);
  shadow.pasteover(r,0,0);
  shadow.blur(2,1);
  return shadow;
 }

 public static Image24 rainbowtext(Font f,String s)
 {
  Image24 r=rendertext(f,s,1,1,1,1);
  r.replacegradient(color(0,0,0,255),0,0,r.sx,r.sy,cBlack,cWhite);
  r.linearmod(3,0,3,0,3,0,1,0);
  r.pseudocolor();
  r.colorreplace(r.get(0,0),cGlass);
  return r;
 }

 public static Image24 swebheader(String s,int size)
 {
  int shift=10;
  Font f=new Font("Courier new",Font.BOLD,2*size);
  Image24 r=rendertext(f,s,0,shift,0,shift);
  int lblue=color(153,204,255,255);
  int dblue=color(0,0,119,255);
  int shadowc=color(0,0,0,200);
  Image24 shadow=r.makeshadow(shadowc);
  r.replacegradient(cBlack,0,0,r.sx,10,lblue,dblue);
  shadow.translate(shift,shift);
  shadow.wind(8,3);
  shadow.pasteover(r,0,0);
  shadow.blur(2,1);
  shadow=shadow.scale(shadow.sx/2,shadow.sy/2);
  Image24 bg=new Image24(shadow.sx,shadow.sy);
  bg.fill(cWhite);
  bg.pasteover(shadow,0,0);
  return bg;
 }

 public static void main(String[] ps)
 {swebheader("ImageToolz",30).getIC().show();
 }


}
