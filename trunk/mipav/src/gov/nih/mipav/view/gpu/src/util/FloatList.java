/*
 * Copyright (c) 2003 Sun Microsystems, Inc. All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistribution of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistribution in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES,
 * INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN
 * MICROSYSTEMS, INC. ("SUN") AND ITS LICENSORS SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR
 * ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR
 * DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE
 * DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY,
 * ARISING OUT OF THE USE OF OR INABILITY TO USE THIS SOFTWARE, EVEN IF
 * SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that this software is not designed or intended for use
 * in the design, construction, operation or maintenance of any nuclear
 * facility.
 *
 * Sun gratefully acknowledges that this software was originally authored
 * and developed by Kenneth Bradley Russell and Christopher John Kline.
 */

package gov.nih.mipav.view.gpu.src.util;

/** Growable array of floats. */

public class FloatList {
  private static final int DEFAULT_SIZE = 10;

  private float[] data = new float[DEFAULT_SIZE];
  private int numElements;

  public void add(float f) {
    if (numElements == data.length) {
      resize(1 + numElements);
    }
    data[numElements++] = f;
    assert numElements <= data.length;
  }

  public int size() {
    return numElements;
  }

  public float get(int index) {
    if (index >= numElements) {
      throw new ArrayIndexOutOfBoundsException(index);
    }
    return data[index];
  }

  public void put(int index, float val) {
    if (index >= numElements) {
      throw new ArrayIndexOutOfBoundsException(index);
    }
    data[index] = val;
  }

  public void trim() {
    if (data.length > numElements) {
      float[] newData = new float[numElements];
      System.arraycopy(data, 0, newData, 0, numElements);
      data = newData;
    }
  }

  public float[] getData() {
    return data;
  }

  private void resize(int minCapacity) {
    int newCapacity = 2 * data.length;
    if (newCapacity == 0) {
      newCapacity = DEFAULT_SIZE;
    }
    if (newCapacity < minCapacity) {
      newCapacity = minCapacity;
    }
    float[] newData = new float[newCapacity];
    System.arraycopy(data, 0, newData, 0, data.length);
    data = newData;
  }
}
