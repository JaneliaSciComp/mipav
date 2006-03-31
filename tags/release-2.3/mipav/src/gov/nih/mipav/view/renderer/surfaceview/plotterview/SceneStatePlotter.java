package gov.nih.mipav.view.renderer.surfaceview.plotterview;

import java.io.*;
import javax.media.j3d.*;

public class SceneStatePlotter implements Serializable {
  public int z;
  public boolean zVisible;
  /** Current sceneRoot transform. */
  public Transform3D transform = null;

  public SceneStatePlotter(int z, boolean zVisible) {
    this.z = z;
    this.zVisible = zVisible;
  }

  public String toString() {
    return ("SceneStatePlotter[" + z + "," + zVisible + "]");
  }

}
