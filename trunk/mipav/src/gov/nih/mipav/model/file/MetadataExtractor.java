package gov.nih.mipav.model.file;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.io.Serializable;
import java.io.PrintStream;
import java.io.PrintWriter;


import java.nio.charset.Charset;

import java.lang.reflect.Array;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Collection;

import java.math.RoundingMode;






/*
 * Ported to MIPAV by William Gandler
 * Copyright 2002-2019 Drew Noakes and contributors
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * More information about this project is available at:
 *
 *    https://drewnoakes.com/code/exif/
 *    https://github.com/drewnoakes/metadata-extractor
 */


public class MetadataExtractor {
	
	public MetadataExtractor() {
		
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public @interface NotNull
	{
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public @interface Nullable
	{
	}

	/**
	 * Used to suppress specific code analysis warnings produced by the Findbugs tool.
	 *
	 * @author Andreas Ziermann
	 */
	public @interface SuppressWarnings
	{
	    /**
	     * The name of the warning to be suppressed.
	     * @return The name of the warning to be suppressed.
	     */
	    @NotNull String value();

	    /**
	     * An explanation of why it is valid to suppress the warning in a particular situation/context.
	     * @return An explanation of why it is valid to suppress the warning in a particular situation/context.
	     */
	    @NotNull String justification();
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public final class StringValue
	{
	    @NotNull
	    private final byte[] _bytes;

	    @Nullable
	    private final Charset _charset;

	    public StringValue(@NotNull byte[] bytes, @Nullable Charset charset)
	    {
	        _bytes = bytes;
	        _charset = charset;
	    }

	    @NotNull
	    public byte[] getBytes()
	    {
	        return _bytes;
	    }

	    @Nullable
	    public Charset getCharset()
	    {
	        return _charset;
	    }

	    @Override
	    public String toString()
	    {
	        return toString(_charset);
	    }

	    public String toString(@Nullable Charset charset)
	    {
	        if (charset != null) {
	            try {
	                return new String(_bytes, charset.name());
	            } catch (UnsupportedEncodingException ex) {
	                // fall through
	            }
	        }

	        return new String(_bytes);
	    }
	}
	    
	    /**
	     * @author Drew Noakes https://drewnoakes.com
	     */
	    //@SuppressWarnings("WeakerAccess")
	    public abstract class SequentialReader
	    {
	        // TODO review whether the masks are needed (in both this and RandomAccessReader)

	        private boolean _isMotorolaByteOrder = true;

	        public abstract long getPosition() throws IOException;

	        /**
	         * Gets the next byte in the sequence.
	         *
	         * @return The read byte value
	         */
	        public abstract byte getByte() throws IOException;

	        /**
	         * Returns the required number of bytes from the sequence.
	         *
	         * @param count The number of bytes to be returned
	         * @return The requested bytes
	         */
	        @NotNull
	        public abstract byte[] getBytes(int count) throws IOException;

	        /**
	         * Retrieves bytes, writing them into a caller-provided buffer.
	         * @param buffer The array to write bytes to.
	         * @param offset The starting position within buffer to write to.
	         * @param count The number of bytes to be written.
	         */
	        public abstract void getBytes(@NotNull byte[] buffer, int offset, int count) throws IOException;

	        /**
	         * Skips forward in the sequence. If the sequence ends, an {@link EOFException} is thrown.
	         *
	         * @param n the number of byte to skip. Must be zero or greater.
	         * @throws EOFException the end of the sequence is reached.
	         * @throws IOException an error occurred reading from the underlying source.
	         */
	        public abstract void skip(long n) throws IOException;

	        /**
	         * Skips forward in the sequence, returning a boolean indicating whether the skip succeeded, or whether the sequence ended.
	         *
	         * @param n the number of byte to skip. Must be zero or greater.
	         * @return a boolean indicating whether the skip succeeded, or whether the sequence ended.
	         * @throws IOException an error occurred reading from the underlying source.
	         */
	        public abstract boolean trySkip(long n) throws IOException;

	        /**
	         * Returns an estimate of the number of bytes that can be read (or skipped
	         * over) from this {@link SequentialReader} without blocking by the next
	         * invocation of a method for this input stream. A single read or skip of
	         * this many bytes will not block, but may read or skip fewer bytes.
	         * <p>
	         * Note that while some implementations of {@link SequentialReader} like
	         * {@link SequentialByteArrayReader} will return the total remaining number
	         * of bytes in the stream, others will not. It is never correct to use the
	         * return value of this method to allocate a buffer intended to hold all
	         * data in this stream.
	         *
	         * @return an estimate of the number of bytes that can be read (or skipped
	         *         over) from this {@link SequentialReader} without blocking or
	         *         {@code 0} when it reaches the end of the input stream.
	         */
	        public abstract int available();

	        /**
	         * Sets the endianness of this reader.
	         * <ul>
	         * <li><code>true</code> for Motorola (or big) endianness (also known as network byte order), with MSB before LSB.</li>
	         * <li><code>false</code> for Intel (or little) endianness, with LSB before MSB.</li>
	         * </ul>
	         *
	         * @param motorolaByteOrder <code>true</code> for Motorola/big endian, <code>false</code> for Intel/little endian
	         */
	        public void setMotorolaByteOrder(boolean motorolaByteOrder)
	        {
	            _isMotorolaByteOrder = motorolaByteOrder;
	        }

	        /**
	         * Gets the endianness of this reader.
	         * <ul>
	         * <li><code>true</code> for Motorola (or big) endianness (also known as network byte order), with MSB before LSB.</li>
	         * <li><code>false</code> for Intel (or little) endianness, with LSB before MSB.</li>
	         * </ul>
	         */
	        public boolean isMotorolaByteOrder()
	        {
	            return _isMotorolaByteOrder;
	        }

	        /**
	         * Returns an unsigned 8-bit int calculated from the next byte of the sequence.
	         *
	         * @return the 8 bit int value, between 0 and 255
	         */
	        public short getUInt8() throws IOException
	        {
	            return (short) (getByte() & 0xFF);
	        }

	        /**
	         * Returns a signed 8-bit int calculated from the next byte the sequence.
	         *
	         * @return the 8 bit int value, between 0x00 and 0xFF
	         */
	        public byte getInt8() throws IOException
	        {
	            return getByte();
	        }

	        /**
	         * Returns an unsigned 16-bit int calculated from the next two bytes of the sequence.
	         *
	         * @return the 16 bit int value, between 0x0000 and 0xFFFF
	         */
	        public int getUInt16() throws IOException
	        {
	            if (_isMotorolaByteOrder) {
	                // Motorola - MSB first
	                return (getByte() << 8 & 0xFF00) |
	                       (getByte()      & 0xFF);
	            } else {
	                // Intel ordering - LSB first
	                return (getByte()      & 0xFF) |
	                       (getByte() << 8 & 0xFF00);
	            }
	        }

	        /**
	         * Returns a signed 16-bit int calculated from two bytes of data (MSB, LSB).
	         *
	         * @return the 16 bit int value, between 0x0000 and 0xFFFF
	         * @throws IOException the buffer does not contain enough bytes to service the request
	         */
	        public short getInt16() throws IOException
	        {
	            if (_isMotorolaByteOrder) {
	                // Motorola - MSB first
	                return (short) (((short)getByte() << 8 & (short)0xFF00) |
	                                ((short)getByte()      & (short)0xFF));
	            } else {
	                // Intel ordering - LSB first
	                return (short) (((short)getByte()      & (short)0xFF) |
	                                ((short)getByte() << 8 & (short)0xFF00));
	            }
	        }

	        /**
	         * Get a 32-bit unsigned integer from the buffer, returning it as a long.
	         *
	         * @return the unsigned 32-bit int value as a long, between 0x00000000 and 0xFFFFFFFF
	         * @throws IOException the buffer does not contain enough bytes to service the request
	         */
	        public long getUInt32() throws IOException
	        {
	            if (_isMotorolaByteOrder) {
	                // Motorola - MSB first (big endian)
	                return (((long)getByte()) << 24 & 0xFF000000L) |
	                       (((long)getByte()) << 16 & 0xFF0000L) |
	                       (((long)getByte()) << 8  & 0xFF00L) |
	                       (((long)getByte())       & 0xFFL);
	            } else {
	                // Intel ordering - LSB first (little endian)
	                return (((long)getByte())       & 0xFFL) |
	                       (((long)getByte()) << 8  & 0xFF00L) |
	                       (((long)getByte()) << 16 & 0xFF0000L) |
	                       (((long)getByte()) << 24 & 0xFF000000L);
	            }
	        }

	        /**
	         * Returns a signed 32-bit integer from four bytes of data.
	         *
	         * @return the signed 32 bit int value, between 0x00000000 and 0xFFFFFFFF
	         * @throws IOException the buffer does not contain enough bytes to service the request
	         */
	        public int getInt32() throws IOException
	        {
	            if (_isMotorolaByteOrder) {
	                // Motorola - MSB first (big endian)
	                return (getByte() << 24 & 0xFF000000) |
	                       (getByte() << 16 & 0xFF0000) |
	                       (getByte() << 8  & 0xFF00) |
	                       (getByte()       & 0xFF);
	            } else {
	                // Intel ordering - LSB first (little endian)
	                return (getByte()       & 0xFF) |
	                       (getByte() << 8  & 0xFF00) |
	                       (getByte() << 16 & 0xFF0000) |
	                       (getByte() << 24 & 0xFF000000);
	            }
	        }

	        /**
	         * Get a signed 64-bit integer from the buffer.
	         *
	         * @return the 64 bit int value, between 0x0000000000000000 and 0xFFFFFFFFFFFFFFFF
	         * @throws IOException the buffer does not contain enough bytes to service the request
	         */
	        public long getInt64() throws IOException
	        {
	            if (_isMotorolaByteOrder) {
	                // Motorola - MSB first
	                return ((long)getByte() << 56 & 0xFF00000000000000L) |
	                       ((long)getByte() << 48 & 0xFF000000000000L) |
	                       ((long)getByte() << 40 & 0xFF0000000000L) |
	                       ((long)getByte() << 32 & 0xFF00000000L) |
	                       ((long)getByte() << 24 & 0xFF000000L) |
	                       ((long)getByte() << 16 & 0xFF0000L) |
	                       ((long)getByte() << 8  & 0xFF00L) |
	                       ((long)getByte()       & 0xFFL);
	            } else {
	                // Intel ordering - LSB first
	                return ((long)getByte()       & 0xFFL) |
	                       ((long)getByte() << 8  & 0xFF00L) |
	                       ((long)getByte() << 16 & 0xFF0000L) |
	                       ((long)getByte() << 24 & 0xFF000000L) |
	                       ((long)getByte() << 32 & 0xFF00000000L) |
	                       ((long)getByte() << 40 & 0xFF0000000000L) |
	                       ((long)getByte() << 48 & 0xFF000000000000L) |
	                       ((long)getByte() << 56 & 0xFF00000000000000L);
	            }
	        }

	        /**
	         * Gets a s15.16 fixed point float from the buffer.
	         * <p>
	         * This particular fixed point encoding has one sign bit, 15 numerator bits and 16 denominator bits.
	         *
	         * @return the floating point value
	         * @throws IOException the buffer does not contain enough bytes to service the request
	         */
	        public float getS15Fixed16() throws IOException
	        {
	            if (_isMotorolaByteOrder) {
	                float res = (getByte() & 0xFF) << 8 |
	                            (getByte() & 0xFF);
	                int d =     (getByte() & 0xFF) << 8 |
	                            (getByte() & 0xFF);
	                return (float)(res + d/65536.0);
	            } else {
	                // this particular branch is untested
	                int d =     (getByte() & 0xFF) |
	                            (getByte() & 0xFF) << 8;
	                float res = (getByte() & 0xFF) |
	                            (getByte() & 0xFF) << 8;
	                return (float)(res + d/65536.0);
	            }
	        }

	        public float getFloat32() throws IOException
	        {
	            return Float.intBitsToFloat(getInt32());
	        }

	        public double getDouble64() throws IOException
	        {
	            return Double.longBitsToDouble(getInt64());
	        }

	        @NotNull
	        public String getString(int bytesRequested) throws IOException
	        {
	            return new String(getBytes(bytesRequested));
	        }

	        @NotNull
	        public String getString(int bytesRequested, String charset) throws IOException
	        {
	            byte[] bytes = getBytes(bytesRequested);
	            try {
	                return new String(bytes, charset);
	            } catch (UnsupportedEncodingException e) {
	                return new String(bytes);
	            }
	        }

	        @NotNull
	        public String getString(int bytesRequested, @NotNull Charset charset) throws IOException
	        {
	            byte[] bytes = getBytes(bytesRequested);
	            return new String(bytes, charset);
	        }

	        @NotNull
	        public StringValue getStringValue(int bytesRequested, @Nullable Charset charset) throws IOException
	        {
	            return new StringValue(getBytes(bytesRequested), charset);
	        }

	        /**
	         * Creates a String from the stream, ending where <code>byte=='\0'</code> or where <code>length==maxLength</code>.
	         *
	         * @param maxLengthBytes The maximum number of bytes to read.  If a zero-byte is not reached within this limit,
	         *                       reading will stop and the string will be truncated to this length.
	         * @return The read string.
	         * @throws IOException The buffer does not contain enough bytes to satisfy this request.
	         */
	        @NotNull
	        public String getNullTerminatedString(int maxLengthBytes, Charset charset) throws IOException
	        {
	           return getNullTerminatedStringValue(maxLengthBytes, charset).toString();
	        }

	        /**
	         * Creates a String from the stream, ending where <code>byte=='\0'</code> or where <code>length==maxLength</code>.
	         *
	         * @param maxLengthBytes The maximum number of bytes to read.  If a <code>\0</code> byte is not reached within this limit,
	         *                       reading will stop and the string will be truncated to this length.
	         * @param charset The <code>Charset</code> to register with the returned <code>StringValue</code>, or <code>null</code> if the encoding
	         *                is unknown
	         * @return The read string.
	         * @throws IOException The buffer does not contain enough bytes to satisfy this request.
	         */
	        @NotNull
	        public StringValue getNullTerminatedStringValue(int maxLengthBytes, Charset charset) throws IOException
	        {
	            byte[] bytes = getNullTerminatedBytes(maxLengthBytes);

	            return new StringValue(bytes, charset);
	        }

	        /**
	         * Returns the sequence of bytes punctuated by a <code>\0</code> value.
	         *
	         * @param maxLengthBytes The maximum number of bytes to read. If a <code>\0</code> byte is not reached within this limit,
	         * the returned array will be <code>maxLengthBytes</code> long.
	         * @return The read byte array, excluding the null terminator.
	         * @throws IOException The buffer does not contain enough bytes to satisfy this request.
	         */
	        @NotNull
	        public byte[] getNullTerminatedBytes(int maxLengthBytes) throws IOException
	        {
	            byte[] buffer = new byte[maxLengthBytes];

	            // Count the number of non-null bytes
	            int length = 0;
	            while (length < buffer.length && (buffer[length] = getByte()) != 0)
	                length++;

	            if (length == maxLengthBytes)
	                return buffer;

	            byte[] bytes = new byte[length];
	            if (length > 0)
	                System.arraycopy(buffer, 0, bytes, 0, length);
	            return bytes;
	        }
	    }


	
	/**
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class StreamReader extends SequentialReader
	{
	    @NotNull
	    private final InputStream _stream;

	    private long _pos;

	    @Override
	    public long getPosition()
	    {
	        return _pos;
	    }

	    //@SuppressWarnings("ConstantConditions")
	    public StreamReader(@NotNull InputStream stream)
	    {
	        if (stream == null)
	            throw new NullPointerException();

	        _stream = stream;
	        _pos = 0;
	    }

	    @Override
	    public byte getByte() throws IOException
	    {
	        int value = _stream.read();
	        if (value == -1)
	            throw new EOFException("End of data reached.");
	        _pos++;
	        return (byte)value;
	    }

	    @NotNull
	    @Override
	    public byte[] getBytes(int count) throws IOException
	    {
	        try {
	            byte[] bytes = new byte[count];
	            getBytes(bytes, 0, count);
	            return bytes;
	        } catch (OutOfMemoryError e) {
	            throw new EOFException("End of data reached.");
	        }

	    }

	    @Override
	    public void getBytes(@NotNull byte[] buffer, int offset, int count) throws IOException
	    {
	        int totalBytesRead = 0;
	        while (totalBytesRead != count)
	        {
	            final int bytesRead = _stream.read(buffer, offset + totalBytesRead, count - totalBytesRead);
	            if (bytesRead == -1)
	                throw new EOFException("End of data reached.");
	            totalBytesRead += bytesRead;
	            assert(totalBytesRead <= count);
	        }
	        _pos += totalBytesRead;
	    }

	    @Override
	    public void skip(long n) throws IOException
	    {
	        if (n < 0)
	            throw new IllegalArgumentException("n must be zero or greater.");

	        long skippedCount = skipInternal(n);

	        if (skippedCount != n)
	            throw new EOFException(String.format("Unable to skip. Requested %d bytes but only %d remained.", n, skippedCount));
	    }

	    @Override
	    public boolean trySkip(long n) throws IOException
	    {
	        if (n < 0)
	            throw new IllegalArgumentException("n must be zero or greater.");

	        return skipInternal(n) == n;
	    }

	    @Override
	    public int available() {
	        try {
	            return _stream.available();
	        } catch (IOException e) {
	            return 0;
	        }
	    }

	    private long skipInternal(long n) throws IOException
	    {
	        // It seems that for some streams, such as BufferedInputStream, that skip can return
	        // some smaller number than was requested. So loop until we either skip enough, or
	        // InputStream.skip returns zero.
	        //
	        // See http://stackoverflow.com/questions/14057720/robust-skipping-of-data-in-a-java-io-inputstream-and-its-subtypes
	        //
	        long skippedTotal = 0;
	        while (skippedTotal != n) {
	            long skipped = _stream.skip(n - skippedTotal);
	            skippedTotal += skipped;
	            if (skipped == 0)
	                break;
	        }
	        _pos += skippedTotal;
	        return skippedTotal;
	    }
	}
	
	/**
	 * Immutable class for holding a rational number without loss of precision.  Provides
	 * a familiar representation via {@link Rational#toString} in form <code>numerator/denominator</code>.
	 *
	 * Note that any value with a numerator of zero will be treated as zero, even if the
	 * denominator is also zero.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	//@SuppressWarnings("WeakerAccess")
	public class Rational extends java.lang.Number implements Comparable<Rational>, Serializable
	{
	    private static final long serialVersionUID = 510688928138848770L;

	    /** Holds the numerator. */
	    private final long _numerator;

	    /** Holds the denominator. */
	    private final long _denominator;

	    /**
	     * Creates a new instance of Rational.  Rational objects are immutable, so
	     * once you've set your numerator and denominator values here, you're stuck
	     * with them!
	     */
	    public Rational(long numerator, long denominator)
	    {
	        _numerator = numerator;
	        _denominator = denominator;
	    }

	    /**
	     * Returns the value of the specified number as a <code>double</code>.
	     * This may involve rounding.
	     *
	     * @return the numeric value represented by this object after conversion
	     *         to type <code>double</code>.
	     */
	    @Override
	    public double doubleValue()
	    {
	        return _numerator == 0
	            ? 0.0
	            : (double) _numerator / (double) _denominator;
	    }

	    /**
	     * Returns the value of the specified number as a <code>float</code>.
	     * This may involve rounding.
	     *
	     * @return the numeric value represented by this object after conversion
	     *         to type <code>float</code>.
	     */
	    @Override
	    public float floatValue()
	    {
	        return _numerator == 0
	            ? 0.0f
	            : (float) _numerator / (float) _denominator;
	    }

	    /**
	     * Returns the value of the specified number as a <code>byte</code>.
	     * This may involve rounding or truncation.  This implementation simply
	     * casts the result of {@link Rational#doubleValue} to <code>byte</code>.
	     *
	     * @return the numeric value represented by this object after conversion
	     *         to type <code>byte</code>.
	     */
	    @Override
	    public final byte byteValue()
	    {
	        return (byte) doubleValue();
	    }

	    /**
	     * Returns the value of the specified number as an <code>int</code>.
	     * This may involve rounding or truncation.  This implementation simply
	     * casts the result of {@link Rational#doubleValue} to <code>int</code>.
	     *
	     * @return the numeric value represented by this object after conversion
	     *         to type <code>int</code>.
	     */
	    @Override
	    public final int intValue()
	    {
	        return (int) doubleValue();
	    }

	    /**
	     * Returns the value of the specified number as a <code>long</code>.
	     * This may involve rounding or truncation.  This implementation simply
	     * casts the result of {@link Rational#doubleValue} to <code>long</code>.
	     *
	     * @return the numeric value represented by this object after conversion
	     *         to type <code>long</code>.
	     */
	    @Override
	    public final long longValue()
	    {
	        return (long) doubleValue();
	    }

	    /**
	     * Returns the value of the specified number as a <code>short</code>.
	     * This may involve rounding or truncation.  This implementation simply
	     * casts the result of {@link Rational#doubleValue} to <code>short</code>.
	     *
	     * @return the numeric value represented by this object after conversion
	     *         to type <code>short</code>.
	     */
	    @Override
	    public final short shortValue()
	    {
	        return (short) doubleValue();
	    }


	    /** Returns the denominator. */
	    public final long getDenominator()
	    {
	        return this._denominator;
	    }

	    /** Returns the numerator. */
	    public final long getNumerator()
	    {
	        return this._numerator;
	    }

	    /**
	     * Returns the reciprocal value of this object as a new Rational.
	     *
	     * @return the reciprocal in a new object
	     */
	    @NotNull
	    public Rational getReciprocal()
	    {
	        return new Rational(this._denominator, this._numerator);
	    }

	    /**
	     * Returns the absolute value of this object as a new Rational.
	     *
	     * @return the absolute value in a new object
	     */
	    public Rational getAbsolute()
	    {
	        return new Rational(Math.abs(this._numerator), Math.abs(this._denominator));
	    }

	    /** Checks if this {@link Rational} number is an Integer, either positive or negative. */
	    public boolean isInteger()
	    {
	        return _denominator == 1 ||
	                (_denominator != 0 && (_numerator % _denominator == 0)) ||
	                (_denominator == 0 && _numerator == 0);
	    }

	    /** Checks if either the numerator or denominator are zero. */
	    public boolean isZero()
	    {
	        return _numerator == 0 || _denominator == 0;
	    }

	    /** True if the value is non-zero and numerator and denominator are either both positive or both negative. */
	    public boolean isPositive()
	    {
	        return !isZero() && (_numerator > 0 == _denominator > 0);
	    }

	    /**
	     * Returns a string representation of the object of form <code>numerator/denominator</code>.
	     *
	     * @return a string representation of the object.
	     */
	    @Override
	    @NotNull
	    public String toString()
	    {
	        return _numerator + "/" + _denominator;
	    }

	    /** Returns the simplest representation of this {@link Rational}'s value possible. */
	    @NotNull
	    public String toSimpleString(boolean allowDecimal)
	    {
	        if (_denominator == 0 && _numerator != 0) {
	            return toString();
	        } else if (isInteger()) {
	            return Integer.toString(intValue());
	        } else {
	            Rational simplifiedInstance = getSimplifiedInstance();
	            if (allowDecimal) {
	                String doubleString = Double.toString(simplifiedInstance.doubleValue());
	                if (doubleString.length() < 5) {
	                    return doubleString;
	                }
	            }
	            return simplifiedInstance.toString();
	        }
	    }

	    /**
	     * Compares two {@link Rational} instances, returning true if they are mathematically
	     * equivalent (in consistence with {@link Rational#equals(Object)} method).
	     *
	     * @param that the {@link Rational} to compare this instance to.
	     * @return the value {@code 0} if this {@link Rational} is
	     *         equal to the argument {@link Rational} mathematically; a value less
	     *         than {@code 0} if this {@link Rational} is less
	     *         than the argument {@link Rational}; and a value greater
	     *         than {@code 0} if this {@link Rational} is greater than the argument
	     *         {@link Rational}.
	     */
	    public int compareTo(@NotNull Rational that) {
	        return Double.compare(this.doubleValue(), that.doubleValue());
	    }

	    /**
	     * Indicates whether this instance and <code>other</code> are numerically equal,
	     * even if their representations differ.
	     *
	     * For example, 1/2 is equal to 10/20 by this method.
	     * Similarly, 1/0 is equal to 100/0 by this method.
	     * To test equal representations, use EqualsExact.
	     *
	     * @param other The rational value to compare with
	     */
	    public boolean equals(Rational other) {
	        return other.doubleValue() == doubleValue();
	    }

	    /**
	     * Indicates whether this instance and <code>other</code> have identical
	     * Numerator and Denominator.
	     * <p>
	     * For example, 1/2 is not equal to 10/20 by this method.
	     * Similarly, 1/0 is not equal to 100/0 by this method.
	     * To test numerically equivalence, use Equals(Rational).</p>
	     *
	     * @param other The rational value to compare with
	     */
	    public boolean equalsExact(Rational other) {
	        return getDenominator() == other.getDenominator() && getNumerator() == other.getNumerator();
	    }

	    /**
	     * Compares two {@link Rational} instances, returning true if they are mathematically
	     * equivalent.
	     *
	     * @param obj the {@link Rational} to compare this instance to.
	     * @return true if instances are mathematically equivalent, otherwise false.  Will also
	     *         return false if <code>obj</code> is not an instance of {@link Rational}.
	     */
	    @Override
	    public boolean equals(@Nullable Object obj)
	    {
	        if (!(obj instanceof Rational))
	            return false;
	        Rational that = (Rational) obj;
	        return this.doubleValue() == that.doubleValue();
	    }

	    @Override
	    public int hashCode()
	    {
	        return (23 * (int)_denominator) + (int)_numerator;
	    }

	    /**
	     * <p>
	     * Simplifies the representation of this {@link Rational} number.</p>
	     * <p>
	     * For example, 5/10 simplifies to 1/2 because both Numerator
	     * and Denominator share a common factor of 5.</p>
	     * <p>
	     * Uses the Euclidean Algorithm to find the greatest common divisor.</p>
	     *
	     * @return A simplified instance if one exists, otherwise a copy of the original value.
	     */
	    @NotNull
	    public Rational getSimplifiedInstance()
	    {
	        long n = _numerator;
	        long d = _denominator;

	        if (d < 0) {
	            n = -n;
	            d = -d;
	        }

	        long gcd = GCD(n, d);

	        return new Rational(n / gcd, d / gcd);
	    }

	    private long GCD(long a, long b)
	    {
	        if (a < 0)
	            a = -a;
	        if (b < 0)
	            b = -b;

	        while (a != 0 && b != 0)
	        {
	            if (a > b)
	                a %= b;
	            else
	                b %= a;
	        }

	        return a == 0 ? b : a;
	    }
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public final static class StringUtil
	{
	    @NotNull
	    public static String join(@NotNull Iterable<? extends CharSequence> strings, @NotNull String delimiter)
	    {
	        int capacity = 0;
	        int delimLength = delimiter.length();

	        Iterator<? extends CharSequence> iter = strings.iterator();
	        if (iter.hasNext())
	            capacity += iter.next().length() + delimLength;

	        StringBuilder buffer = new StringBuilder(capacity);
	        iter = strings.iterator();
	        if (iter.hasNext()) {
	            buffer.append(iter.next());
	            while (iter.hasNext()) {
	                buffer.append(delimiter);
	                buffer.append(iter.next());
	            }
	        }
	        return buffer.toString();
	    }

	    @NotNull
	    public static <T extends CharSequence> String join(@NotNull T[] strings, @NotNull String delimiter)
	    {
	        int capacity = 0;
	        int delimLength = delimiter.length();
	        for (T value : strings)
	            capacity += value.length() + delimLength;

	        StringBuilder buffer = new StringBuilder(capacity);
	        boolean first = true;
	        for (T value : strings) {
	            if (!first) {
	                buffer.append(delimiter);
	            } else {
	                first = false;
	            }
	            buffer.append(value);
	        }
	        return buffer.toString();
	    }

	    @NotNull
	    public static String fromStream(@NotNull InputStream stream) throws IOException
	    {
	        BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
	        StringBuilder sb = new StringBuilder();
	        String line;
	        while ((line = reader.readLine()) != null) {
	            sb.append(line);
	        }
	        return sb.toString();
	    }

	    public static int compare(@Nullable String s1, @Nullable String s2)
	    {
	        boolean null1 = s1 == null;
	        boolean null2 = s2 == null;

	        if (null1 && null2) {
	            return 0;
	        } else if (null1) {
	            return -1;
	        } else if (null2) {
	            return 1;
	        } else {
	            return s1.compareTo(s2);
	        }
	    }

	    @NotNull
	    public static String urlEncode(@NotNull String name)
	    {
	        // Sufficient for now, it seems
	        return name.replace(" ", "%20");
	    }
	}

	
	/**
	 * Models a particular tag within a {@link com.drew.metadata.Directory} and provides methods for obtaining its value.
	 * Immutable.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	//@SuppressWarnings("unused")
	public class Tag
	{
	    private final int _tagType;
	    @NotNull
	    private final Directory _directory;

	    public Tag(int tagType, @NotNull Directory directory)
	    {
	        _tagType = tagType;
	        _directory = directory;
	    }

	    /**
	     * Gets the tag type as an int
	     *
	     * @return the tag type as an int
	     */
	    public int getTagType()
	    {
	        return _tagType;
	    }

	    /**
	     * Gets the tag type in hex notation as a String with padded leading
	     * zeroes if necessary (i.e. <code>0x100e</code>).
	     *
	     * @return the tag type as a string in hexadecimal notation
	     */
	    @NotNull
	    public String getTagTypeHex()
	    {
	        return String.format("0x%04x", _tagType);
	    }

	    /**
	     * Get a description of the tag's value, considering enumerated values
	     * and units.
	     *
	     * @return a description of the tag's value
	     */
	    @Nullable
	    public String getDescription()
	    {
	        return _directory.getDescription(_tagType);
	    }

	    /**
	     * Get whether this tag has a name.
	     *
	     * If <code>true</code>, it may be accessed via {@link #getTagName}.
	     * If <code>false</code>, {@link #getTagName} will return a string resembling <code>"Unknown tag (0x1234)"</code>.
	     *
	     * @return whether this tag has a name
	     */
	    public boolean hasTagName()
	    {
	        return _directory.hasTagName(_tagType);
	    }

	    /**
	     * Get the name of the tag, such as <code>Aperture</code>, or
	     * <code>InteropVersion</code>.
	     *
	     * @return the tag's name
	     */
	    @NotNull
	    public String getTagName()
	    {
	        return _directory.getTagName(_tagType);
	    }

	    /**
	     * Get the name of the {@link com.drew.metadata.Directory} in which the tag exists, such as
	     * <code>Exif</code>, <code>GPS</code> or <code>Interoperability</code>.
	     *
	     * @return name of the {@link com.drew.metadata.Directory} in which this tag exists
	     */
	    @NotNull
	    public String getDirectoryName()
	    {
	        return _directory.getName();
	    }

	    /**
	     * A basic representation of the tag's type and value.  EG: <code>[Exif IFD0] FNumber - f/2.8</code>.
	     *
	     * @return the tag's type and value
	     */
	    @Override
	    @NotNull
	    public String toString()
	    {
	        String description = getDescription();
	        if (description == null)
	            description = _directory.getString(getTagType()) + " (unable to formulate description)";
	        return "[" + _directory.getName() + "] " + getTagName() + " - " + description;
	    }
	}
	
	/**
	 * Base class for all tag descriptor classes.  Implementations are responsible for
	 * providing the human-readable string representation of tag values stored in a directory.
	 * The directory is provided to the tag descriptor via its constructor.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class TagDescriptor<T extends Directory>
	{
	    @NotNull
	    protected final T _directory;

	    public TagDescriptor(@NotNull T directory)
	    {
	        _directory = directory;
	    }

	    /**
	     * Returns a descriptive value of the specified tag for this image.
	     * Where possible, known values will be substituted here in place of the raw
	     * tokens actually kept in the metadata segment.  If no substitution is
	     * available, the value provided by <code>getString(tagType)</code> will be returned.
	     *
	     * @param tagType the tag to find a description for
	     * @return a description of the image's value for the specified tag, or
	     *         <code>null</code> if the tag hasn't been defined.
	     */
	    @Nullable
	    public String getDescription(int tagType)
	    {
	        Object object = _directory.getObject(tagType);

	        if (object == null)
	            return null;

	        // special presentation for long arrays
	        if (object.getClass().isArray()) {
	            final int length = Array.getLength(object);
	            if (length > 16) {
	                return String.format("[%d values]", length);
	            }
	        }

	        if (object instanceof Date) {
	            // Produce a date string having a format that includes the offset in form "+00:00"
	            return new SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy")
	                .format((Date) object)
	                .replaceAll("([0-9]{2} [^ ]+)$", ":$1");
	        }

	        // no special handling required, so use default conversion to a string
	        return _directory.getString(tagType);
	    }

	    /**
	     * Takes a series of 4 bytes from the specified offset, and converts these to a
	     * well-known version number, where possible.
	     * <p>
	     * Two different formats are processed:
	     * <ul>
	     * <li>[30 32 31 30] -&gt; 2.10</li>
	     * <li>[0 1 0 0] -&gt; 1.00</li>
	     * </ul>
	     *
	     * @param components  the four version values
	     * @param majorDigits the number of components to be
	     * @return the version as a string of form "2.10" or null if the argument cannot be converted
	     */
	    @Nullable
	    public String convertBytesToVersionString(@Nullable int[] components, final int majorDigits)
	    {
	        if (components == null)
	            return null;
	        StringBuilder version = new StringBuilder();
	        for (int i = 0; i < 4 && i < components.length; i++) {
	            if (i == majorDigits)
	                version.append('.');
	            char c = (char)components[i];
	            if (c < '0')
	                c += '0';
	            if (i == 0 && c == '0')
	                continue;
	            version.append(c);
	        }
	        return version.toString();
	    }

	    @Nullable
	    protected String getVersionBytesDescription(final int tagType, int majorDigits)
	    {
	        int[] values = _directory.getIntArray(tagType);
	        return values == null ? null : convertBytesToVersionString(values, majorDigits);
	    }

	    @Nullable
	    protected String getIndexedDescription(final int tagType, @NotNull String... descriptions)
	    {
	        return getIndexedDescription(tagType, 0, descriptions);
	    }

	    @Nullable
	    protected String getIndexedDescription(final int tagType, final int baseIndex, @NotNull String... descriptions)
	    {
	        final Long index = _directory.getLongObject(tagType);
	        if (index == null)
	            return null;
	        final long arrayIndex = index - baseIndex;
	        if (arrayIndex >= 0 && arrayIndex < (long)descriptions.length) {
	            String description = descriptions[(int)arrayIndex];
	            if (description != null)
	                return description;
	        }
	        return "Unknown (" + index + ")";
	    }

	    @Nullable
	    protected String getByteLengthDescription(final int tagType)
	    {
	        byte[] bytes = _directory.getByteArray(tagType);
	        if (bytes == null)
	            return null;
	        return String.format("(%d byte%s)", bytes.length, bytes.length == 1 ? "" : "s");
	    }

	    @Nullable
	    protected String getSimpleRational(final int tagType)
	    {
	        Rational value = _directory.getRational(tagType);
	        if (value == null)
	            return null;
	        return value.toSimpleString(true);
	    }

	    @Nullable
	    protected String getDecimalRational(final int tagType, final int decimalPlaces)
	    {
	        Rational value = _directory.getRational(tagType);
	        if (value == null)
	            return null;
	        return String.format("%." + decimalPlaces + "f", value.doubleValue());
	    }

	    @Nullable
	    protected String getFormattedInt(final int tagType, @NotNull final String format)
	    {
	        Integer value = _directory.getInteger(tagType);
	        if (value == null)
	            return null;
	        return String.format(format, value);
	    }

	    @Nullable
	    protected String getFormattedFloat(final int tagType, @NotNull final String format)
	    {
	        Float value = _directory.getFloatObject(tagType);
	        if (value == null)
	            return null;
	        return String.format(format, value);
	    }

	    @Nullable
	    protected String getFormattedString(final int tagType, @NotNull final String format)
	    {
	        String value = _directory.getString(tagType);
	        if (value == null)
	            return null;
	        return String.format(format, value);
	    }

	    @Nullable
	    protected String getEpochTimeDescription(final int tagType)
	    {
	        // TODO have observed a byte[8] here which is likely some kind of date (ticks as long?)
	        Long value = _directory.getLongObject(tagType);
	        if (value == null)
	            return null;
	        return new Date(value).toString();
	    }

	    /**
	     * LSB first. Labels may be null, a String, or a String[2] with (low label,high label) values.
	     */
	    @Nullable
	    protected String getBitFlagDescription(final int tagType, @NotNull final Object... labels)
	    {
	        Integer value = _directory.getInteger(tagType);

	        if (value == null)
	            return null;

	        List<String> parts = new ArrayList<String>();

	        int bitIndex = 0;
	        while (labels.length > bitIndex) {
	            Object labelObj = labels[bitIndex];
	            if (labelObj != null) {
	                boolean isBitSet = (value & 1) == 1;
	                if (labelObj instanceof String[]) {
	                    String[] labelPair = (String[])labelObj;
	                    assert(labelPair.length == 2);
	                    parts.add(labelPair[isBitSet ? 1 : 0]);
	                } else if (isBitSet && labelObj instanceof String) {
	                    parts.add((String)labelObj);
	                }
	            }
	            value >>= 1;
	            bitIndex++;
	        }

	        return StringUtil.join(parts, ", ");
	    }

	    @Nullable
	    protected String get7BitStringFromBytes(final int tagType)
	    {
	        final byte[] bytes = _directory.getByteArray(tagType);

	        if (bytes == null)
	            return null;

	        int length = bytes.length;
	        for (int index = 0; index < bytes.length; index++) {
	            int i = bytes[index] & 0xFF;
	            if (i == 0 || i > 0x7F) {
	                length = index;
	                break;
	            }
	        }

	        return new String(bytes, 0, length);
	    }

	    @Nullable
	    protected String getStringFromBytes(int tag, Charset cs)
	    {
	        byte[] values = _directory.getByteArray(tag);

	        if (values == null)
	            return null;

	        try {
	            return new String(values, cs.name()).trim();
	        } catch (UnsupportedEncodingException e) {
	            return null;
	        }
	    }

	    @Nullable
	    protected String getRationalOrDoubleString(int tagType)
	    {
	        Rational rational = _directory.getRational(tagType);
	        if (rational != null)
	            return rational.toSimpleString(true);

	        Double d = _directory.getDoubleObject(tagType);
	        if (d != null) {
	            DecimalFormat format = new DecimalFormat("0.###");
	            return format.format(d);
	        }

	        return null;
	    }

	    @NotNull
	    protected String getFStopDescription(double fStop)
	    {
	        DecimalFormat format = new DecimalFormat("0.0");
	        format.setRoundingMode(RoundingMode.HALF_UP);
	        return "f/" + format.format(fStop);
	    }

	    @NotNull
	    protected String getFocalLengthDescription(double mm)
	    {
	        DecimalFormat format = new DecimalFormat("0.#");
	        format.setRoundingMode(RoundingMode.HALF_UP);
	        return format.format(mm) + " mm";
	    }

	    @Nullable
	    protected String getLensSpecificationDescription(int tag)
	    {
	        Rational[] values = _directory.getRationalArray(tag);

	        if (values == null || values.length != 4 || (values[0].isZero() && values[2].isZero()))
	            return null;

	        StringBuilder sb = new StringBuilder();

	        if (values[0].equals(values[1]))
	            sb.append(values[0].toSimpleString(true)).append("mm");
	        else
	            sb.append(values[0].toSimpleString(true)).append('-').append(values[1].toSimpleString(true)).append("mm");

	        if (!values[2].isZero()) {
	            sb.append(' ');

	            DecimalFormat format = new DecimalFormat("0.0");
	            format.setRoundingMode(RoundingMode.HALF_UP);

	            if (values[2].equals(values[3]))
	                sb.append(getFStopDescription(values[2].doubleValue()));
	            else
	                sb.append("f/").append(format.format(values[2].doubleValue())).append('-').append(format.format(values[3].doubleValue()));
	        }

	        return sb.toString();
	    }

	    @Nullable
	    protected String getOrientationDescription(int tag)
	    {
	        return getIndexedDescription(tag, 1,
	            "Top, left side (Horizontal / normal)",
	            "Top, right side (Mirror horizontal)",
	            "Bottom, right side (Rotate 180)",
	            "Bottom, left side (Mirror vertical)",
	            "Left side, top (Mirror horizontal and rotate 270 CW)",
	            "Right side, top (Rotate 90 CW)",
	            "Right side, bottom (Mirror horizontal and rotate 90 CW)",
	            "Left side, bottom (Rotate 270 CW)");
	    }

	    @Nullable
	    protected String getShutterSpeedDescription(int tag)
	    {
	        // Thanks to Mark Edwards for spotting and patching a bug in the calculation of this
	        // description (spotted bug using a Canon EOS 300D).
	        // Thanks also to Gli Blr for spotting this bug.
	        Float apexValue = _directory.getFloatObject(tag);
	        if (apexValue == null)
	            return null;
	        if (apexValue <= 1) {
	            float apexPower = (float)(1 / (Math.exp(apexValue * Math.log(2))));
	            long apexPower10 = Math.round((double)apexPower * 10.0);
	            float fApexPower = (float)apexPower10 / 10.0f;
	            DecimalFormat format = new DecimalFormat("0.##");
	            format.setRoundingMode(RoundingMode.HALF_UP);
	            return format.format(fApexPower) + " sec";
	        } else {
	            int apexPower = (int)((Math.exp(apexValue * Math.log(2))));
	            return "1/" + apexPower + " sec";
	        }
	    }

	    // EXIF UserComment, GPSProcessingMethod and GPSAreaInformation
	    @Nullable
	    protected String getEncodedTextDescription(int tagType)
	    {
	        byte[] commentBytes = _directory.getByteArray(tagType);
	        if (commentBytes == null)
	            return null;
	        if (commentBytes.length == 0)
	            return "";

	        final Map<String, String> encodingMap = new HashMap<String, String>();
	        encodingMap.put("ASCII", System.getProperty("file.encoding")); // Someone suggested "ISO-8859-1".
	        encodingMap.put("UNICODE", "UTF-16LE");
	        encodingMap.put("JIS", "Shift-JIS"); // We assume this charset for now.  Another suggestion is "JIS".

	        try {
	            if (commentBytes.length >= 10) {
	                String firstTenBytesString = new String(commentBytes, 0, 10);

	                // try each encoding name
	                for (Map.Entry<String, String> pair : encodingMap.entrySet()) {
	                    String encodingName = pair.getKey();
	                    String charset = pair.getValue();
	                    if (firstTenBytesString.startsWith(encodingName)) {
	                        // skip any null or blank characters commonly present after the encoding name, up to a limit of 10 from the start
	                        for (int j = encodingName.length(); j < 10; j++) {
	                            byte b = commentBytes[j];
	                            if (b != '\0' && b != ' ')
	                                return new String(commentBytes, j, commentBytes.length - j, charset).trim();
	                        }
	                        return new String(commentBytes, 10, commentBytes.length - 10, charset).trim();
	                    }
	                }
	            }
	            // special handling fell through, return a plain string representation
	            return new String(commentBytes, System.getProperty("file.encoding")).trim();
	        } catch (UnsupportedEncodingException ex) {
	            return null;
	        }
	    }
	}
	
	/**
	 * Represents a compound exception, as modelled in JDK 1.4, but
	 * unavailable in previous versions.  This class allows support
	 * of these previous JDK versions.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class CompoundException extends Exception
	{
	    private static final long serialVersionUID = -9207883813472069925L;

	    @Nullable
	    private final Throwable _innerException;

	    public CompoundException(@Nullable String msg)
	    {
	        this(msg, null);
	    }

	    public CompoundException(@Nullable Throwable exception)
	    {
	        this(null, exception);
	    }

	    public CompoundException(@Nullable String msg, @Nullable Throwable innerException)
	    {
	        super(msg);
	        _innerException = innerException;
	    }

	    @Nullable
	    public Throwable getInnerException()
	    {
	        return _innerException;
	    }

	    @Override
	    @NotNull
	    public String toString()
	    {
	        StringBuilder string = new StringBuilder();
	        string.append(super.toString());
	        if (_innerException != null) {
	            string.append("\n");
	            string.append("--- inner exception ---");
	            string.append("\n");
	            string.append(_innerException.toString());
	        }
	        return string.toString();
	    }

	    @Override
	    public void printStackTrace(@NotNull PrintStream s)
	    {
	        super.printStackTrace(s);
	        if (_innerException != null) {
	            s.println("--- inner exception ---");
	            _innerException.printStackTrace(s);
	        }
	    }

	    @Override
	    public void printStackTrace(@NotNull PrintWriter s)
	    {
	        super.printStackTrace(s);
	        if (_innerException != null) {
	            s.println("--- inner exception ---");
	            _innerException.printStackTrace(s);
	        }
	    }

	    @Override
	    public void printStackTrace()
	    {
	        super.printStackTrace();
	        if (_innerException != null) {
	            System.err.println("--- inner exception ---");
	            _innerException.printStackTrace();
	        }
	    }
	}


	/**
	 * Base class for all metadata specific exceptions.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class MetadataException extends CompoundException
	{
	    private static final long serialVersionUID = 8612756143363919682L;

	    public MetadataException(@Nullable String msg)
	    {
	        super(msg);
	    }

	    public MetadataException(@Nullable Throwable exception)
	    {
	        super(exception);
	    }

	    public MetadataException(@Nullable String msg, @Nullable Throwable innerException)
	    {
	        super(msg, innerException);
	    }
	}

	
	/**
	 * Abstract base class for all directory implementations, having methods for getting and setting tag values of various
	 * data types.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	@java.lang.SuppressWarnings("WeakerAccess")
	public abstract class Directory
	{
	    private static final String _floatFormatPattern = "0.###";

	    /** Map of values hashed by type identifiers. */
	    @NotNull
	    protected final Map<Integer, Object> _tagMap = new HashMap<Integer, Object>();

	    /**
	     * A convenient list holding tag values in the order in which they were stored.
	     * This is used for creation of an iterator, and for counting the number of
	     * defined tags.
	     */
	    @NotNull
	    protected final Collection<Tag> _definedTagList = new ArrayList<Tag>();

	    @NotNull
	    private final Collection<String> _errorList = new ArrayList<String>(4);

	    /** The descriptor used to interpret tag values. */
	    protected TagDescriptor<?> _descriptor;

	    @Nullable
	    private Directory _parent;

	// ABSTRACT METHODS

	    /**
	     * Provides the name of the directory, for display purposes.  E.g. <code>Exif</code>
	     *
	     * @return the name of the directory
	     */
	    @NotNull
	    public abstract String getName();

	    /**
	     * Provides the map of tag names, hashed by tag type identifier.
	     *
	     * @return the map of tag names
	     */
	    @NotNull
	    protected abstract HashMap<Integer, String> getTagNameMap();

	    protected Directory()
	    {}

	// VARIOUS METHODS

	    /**
	     * Gets a value indicating whether the directory is empty, meaning it contains no errors and no tag values.
	     */
	    public boolean isEmpty()
	    {
	        return _errorList.isEmpty() && _definedTagList.isEmpty();
	    }

	    /**
	     * Indicates whether the specified tag type has been set.
	     *
	     * @param tagType the tag type to check for
	     * @return true if a value exists for the specified tag type, false if not
	     */
	    @java.lang.SuppressWarnings({ "UnnecessaryBoxing" })
	    public boolean containsTag(int tagType)
	    {
	        return _tagMap.containsKey(Integer.valueOf(tagType));
	    }

	    /**
	     * Returns an Iterator of Tag instances that have been set in this Directory.
	     *
	     * @return an Iterator of Tag instances
	     */
	    @NotNull
	    public Collection<Tag> getTags()
	    {
	        return Collections.unmodifiableCollection(_definedTagList);
	    }

	    /**
	     * Returns the number of tags set in this Directory.
	     *
	     * @return the number of tags set in this Directory
	     */
	    public int getTagCount()
	    {
	        return _definedTagList.size();
	    }

	    /**
	     * Sets the descriptor used to interpret tag values.
	     *
	     * @param descriptor the descriptor used to interpret tag values
	     */
	    @java.lang.SuppressWarnings({ "ConstantConditions" })
	    public void setDescriptor(@NotNull TagDescriptor<?> descriptor)
	    {
	        if (descriptor == null)
	            throw new NullPointerException("cannot set a null descriptor");
	        _descriptor = descriptor;
	    }

	    /**
	     * Registers an error message with this directory.
	     *
	     * @param message an error message.
	     */
	    public void addError(@NotNull String message)
	    {
	        _errorList.add(message);
	    }

	    /**
	     * Gets a value indicating whether this directory has any error messages.
	     *
	     * @return true if the directory contains errors, otherwise false
	     */
	    public boolean hasErrors()
	    {
	        return _errorList.size() > 0;
	    }

	    /**
	     * Used to iterate over any error messages contained in this directory.
	     *
	     * @return an iterable collection of error message strings.
	     */
	    @NotNull
	    public Iterable<String> getErrors()
	    {
	        return Collections.unmodifiableCollection(_errorList);
	    }

	    /** Returns the count of error messages in this directory. */
	    public int getErrorCount()
	    {
	        return _errorList.size();
	    }

	    @Nullable
	    public Directory getParent()
	    {
	        return _parent;
	    }

	    public void setParent(@NotNull Directory parent)
	    {
	        _parent = parent;
	    }

	// TAG SETTERS

	    /**
	     * Sets an <code>int</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as an int
	     */
	    public void setInt(int tagType, int value)
	    {
	        setObject(tagType, value);
	    }

	    /**
	     * Sets an <code>int[]</code> (array) for the specified tag.
	     *
	     * @param tagType the tag identifier
	     * @param ints    the int array to store
	     */
	    public void setIntArray(int tagType, @NotNull int[] ints)
	    {
	        setObjectArray(tagType, ints);
	    }

	    /**
	     * Sets a <code>float</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a float
	     */
	    public void setFloat(int tagType, float value)
	    {
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>float[]</code> (array) for the specified tag.
	     *
	     * @param tagType the tag identifier
	     * @param floats  the float array to store
	     */
	    public void setFloatArray(int tagType, @NotNull float[] floats)
	    {
	        setObjectArray(tagType, floats);
	    }

	    /**
	     * Sets a <code>double</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a double
	     */
	    public void setDouble(int tagType, double value)
	    {
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>double[]</code> (array) for the specified tag.
	     *
	     * @param tagType the tag identifier
	     * @param doubles the double array to store
	     */
	    public void setDoubleArray(int tagType, @NotNull double[] doubles)
	    {
	        setObjectArray(tagType, doubles);
	    }

	    /**
	     * Sets a <code>StringValue</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a StringValue
	     */
	    @java.lang.SuppressWarnings({ "ConstantConditions" })
	    public void setStringValue(int tagType, @NotNull StringValue value)
	    {
	        if (value == null)
	            throw new NullPointerException("cannot set a null StringValue");
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>String</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a String
	     */
	    @java.lang.SuppressWarnings({ "ConstantConditions" })
	    public void setString(int tagType, @NotNull String value)
	    {
	        if (value == null)
	            throw new NullPointerException("cannot set a null String");
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>String[]</code> (array) for the specified tag.
	     *
	     * @param tagType the tag identifier
	     * @param strings the String array to store
	     */
	    public void setStringArray(int tagType, @NotNull String[] strings)
	    {
	        setObjectArray(tagType, strings);
	    }

	    /**
	     * Sets a <code>StringValue[]</code> (array) for the specified tag.
	     *
	     * @param tagType the tag identifier
	     * @param strings the StringValue array to store
	     */
	    public void setStringValueArray(int tagType, @NotNull StringValue[] strings)
	    {
	        setObjectArray(tagType, strings);
	    }

	    /**
	     * Sets a <code>boolean</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a boolean
	     */
	    public void setBoolean(int tagType, boolean value)
	    {
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>long</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a long
	     */
	    public void setLong(int tagType, long value)
	    {
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>java.util.Date</code> value for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag as a java.util.Date
	     */
	    public void setDate(int tagType, @NotNull java.util.Date value)
	    {
	        setObject(tagType, value);
	    }

	    /**
	     * Sets a <code>Rational</code> value for the specified tag.
	     *
	     * @param tagType  the tag's value as an int
	     * @param rational rational number
	     */
	    public void setRational(int tagType, @NotNull Rational rational)
	    {
	        setObject(tagType, rational);
	    }

	    /**
	     * Sets a <code>Rational[]</code> (array) for the specified tag.
	     *
	     * @param tagType   the tag identifier
	     * @param rationals the Rational array to store
	     */
	    public void setRationalArray(int tagType, @NotNull Rational[] rationals)
	    {
	        setObjectArray(tagType, rationals);
	    }

	    /**
	     * Sets a <code>byte[]</code> (array) for the specified tag.
	     *
	     * @param tagType the tag identifier
	     * @param bytes   the byte array to store
	     */
	    public void setByteArray(int tagType, @NotNull byte[] bytes)
	    {
	        setObjectArray(tagType, bytes);
	    }

	    /**
	     * Sets a <code>Object</code> for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param value   the value for the specified tag
	     * @throws NullPointerException if value is <code>null</code>
	     */
	    @java.lang.SuppressWarnings( { "ConstantConditions", "UnnecessaryBoxing" })
	    public void setObject(int tagType, @NotNull Object value)
	    {
	        if (value == null)
	            throw new NullPointerException("cannot set a null object");

	        if (!_tagMap.containsKey(Integer.valueOf(tagType))) {
	            _definedTagList.add(new Tag(tagType, this));
	        }
//	        else {
//	            final Object oldValue = _tagMap.get(tagType);
//	            if (!oldValue.equals(value))
//	                addError(String.format("Overwritten tag 0x%s (%s).  Old=%s, New=%s", Integer.toHexString(tagType), getTagName(tagType), oldValue, value));
//	        }
	        _tagMap.put(tagType, value);
	    }

	    /**
	     * Sets an array <code>Object</code> for the specified tag.
	     *
	     * @param tagType the tag's value as an int
	     * @param array   the array of values for the specified tag
	     */
	    public void setObjectArray(int tagType, @NotNull Object array)
	    {
	        // for now, we don't do anything special -- this method might be a candidate for removal once the dust settles
	        setObject(tagType, array);
	    }

	// TAG GETTERS

	    /**
	     * Returns the specified tag's value as an int, if possible.  Every attempt to represent the tag's value as an int
	     * is taken.  Here is a list of the action taken depending upon the tag's original type:
	     * <ul>
	     * <li> int - Return unchanged.
	     * <li> Number - Return an int value (real numbers are truncated).
	     * <li> Rational - Truncate any fractional part and returns remaining int.
	     * <li> String - Attempt to parse string as an int.  If this fails, convert the char[] to an int (using shifts and OR).
	     * <li> Rational[] - Return int value of first item in array.
	     * <li> byte[] - Return int value of first item in array.
	     * <li> int[] - Return int value of first item in array.
	     * </ul>
	     *
	     * @throws MetadataException if no value exists for tagType or if it cannot be converted to an int.
	     */
	    public int getInt(int tagType) throws MetadataException
	    {
	        Integer integer = getInteger(tagType);
	        if (integer!=null)
	            return integer;

	        Object o = getObject(tagType);
	        if (o == null)
	            throw new MetadataException("Tag '" + getTagName(tagType) + "' has not been set -- check using containsTag() first");
	        throw new MetadataException("Tag '" + tagType + "' cannot be converted to int.  It is of type '" + o.getClass() + "'.");
	    }

	    /**
	     * Returns the specified tag's value as an Integer, if possible.  Every attempt to represent the tag's value as an
	     * Integer is taken.  Here is a list of the action taken depending upon the tag's original type:
	     * <ul>
	     * <li> int - Return unchanged
	     * <li> Number - Return an int value (real numbers are truncated)
	     * <li> Rational - Truncate any fractional part and returns remaining int
	     * <li> String - Attempt to parse string as an int.  If this fails, convert the char[] to an int (using shifts and OR)
	     * <li> Rational[] - Return int value of first item in array if length &gt; 0
	     * <li> byte[] - Return int value of first item in array if length &gt; 0
	     * <li> int[] - Return int value of first item in array if length &gt; 0
	     * </ul>
	     *
	     * If the value is not found or cannot be converted to int, <code>null</code> is returned.
	     */
	    @Nullable
	    public Integer getInteger(int tagType)
	    {
	        Object o = getObject(tagType);

	        if (o == null)
	            return null;

	        if (o instanceof Number) {
	            return ((Number)o).intValue();
	        } else if (o instanceof String || o instanceof StringValue) {
	            try {
	                return Integer.parseInt(o.toString());
	            } catch (NumberFormatException nfe) {
	                // convert the char array to an int
	                String s = o.toString();
	                byte[] bytes = s.getBytes();
	                long val = 0;
	                for (byte aByte : bytes) {
	                    val = val << 8;
	                    val += (aByte & 0xff);
	                }
	                return (int)val;
	            }
	        } else if (o instanceof Rational[]) {
	            Rational[] rationals = (Rational[])o;
	            if (rationals.length == 1)
	                return rationals[0].intValue();
	        } else if (o instanceof byte[]) {
	            byte[] bytes = (byte[])o;
	            if (bytes.length == 1)
	                return (int)bytes[0];
	        } else if (o instanceof int[]) {
	            int[] ints = (int[])o;
	            if (ints.length == 1)
	                return ints[0];
	        } else if (o instanceof short[]) {
	            short[] shorts = (short[])o;
	            if (shorts.length == 1)
	                return (int)shorts[0];
	        }
	        return null;
	    }

	    /**
	     * Gets the specified tag's value as a String array, if possible.  Only supported
	     * where the tag is set as StringValue[], String[], StringValue, String, int[], byte[] or Rational[].
	     *
	     * @param tagType the tag identifier
	     * @return the tag's value as an array of Strings. If the value is unset or cannot be converted, <code>null</code> is returned.
	     */
	    @Nullable
	    public String[] getStringArray(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof String[])
	            return (String[])o;
	        if (o instanceof String)
	            return new String[] { (String)o };
	        if (o instanceof StringValue)
	            return new String[] { o.toString() };
	        if (o instanceof StringValue[]) {
	            StringValue[] stringValues = (StringValue[])o;
	            String[] strings = new String[stringValues.length];
	            for (int i = 0; i < strings.length; i++)
	                strings[i] = stringValues[i].toString();
	            return strings;
	        }
	        if (o instanceof int[]) {
	            int[] ints = (int[])o;
	            String[] strings = new String[ints.length];
	            for (int i = 0; i < strings.length; i++)
	                strings[i] = Integer.toString(ints[i]);
	            return strings;
	        }
	        if (o instanceof byte[]) {
	            byte[] bytes = (byte[])o;
	            String[] strings = new String[bytes.length];
	            for (int i = 0; i < strings.length; i++)
	                strings[i] = Byte.toString(bytes[i]);
	            return strings;
	        }
	        if (o instanceof Rational[]) {
	            Rational[] rationals = (Rational[])o;
	            String[] strings = new String[rationals.length];
	            for (int i = 0; i < strings.length; i++)
	                strings[i] = rationals[i].toSimpleString(false);
	            return strings;
	        }
	        return null;
	    }

	    /**
	     * Gets the specified tag's value as a StringValue array, if possible.
	     * Only succeeds if the tag is set as StringValue[], or StringValue.
	     *
	     * @param tagType the tag identifier
	     * @return the tag's value as an array of StringValues. If the value is unset or cannot be converted, <code>null</code> is returned.
	     */
	    @Nullable
	    public StringValue[] getStringValueArray(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof StringValue[])
	            return (StringValue[])o;
	        if (o instanceof StringValue)
	            return new StringValue[] {(StringValue) o};
	        return null;
	    }

	    /**
	     * Gets the specified tag's value as an int array, if possible.  Only supported
	     * where the tag is set as String, Integer, int[], byte[] or Rational[].
	     *
	     * @param tagType the tag identifier
	     * @return the tag's value as an int array
	     */
	    @Nullable
	    public int[] getIntArray(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof int[])
	            return (int[])o;
	        if (o instanceof Rational[]) {
	            Rational[] rationals = (Rational[])o;
	            int[] ints = new int[rationals.length];
	            for (int i = 0; i < ints.length; i++) {
	                ints[i] = rationals[i].intValue();
	            }
	            return ints;
	        }
	        if (o instanceof short[]) {
	            short[] shorts = (short[])o;
	            int[] ints = new int[shorts.length];
	            for (int i = 0; i < shorts.length; i++) {
	                ints[i] = shorts[i];
	            }
	            return ints;
	        }
	        if (o instanceof byte[]) {
	            byte[] bytes = (byte[])o;
	            int[] ints = new int[bytes.length];
	            for (int i = 0; i < bytes.length; i++) {
	                ints[i] = bytes[i];
	            }
	            return ints;
	        }
	        if (o instanceof CharSequence) {
	            CharSequence str = (CharSequence)o;
	            int[] ints = new int[str.length()];
	            for (int i = 0; i < str.length(); i++) {
	                ints[i] = str.charAt(i);
	            }
	            return ints;
	        }
	        if (o instanceof Integer)
	            return new int[] { (Integer)o };

	        return null;
	    }

	    /**
	     * Gets the specified tag's value as an byte array, if possible.  Only supported
	     * where the tag is set as String, Integer, int[], byte[] or Rational[].
	     *
	     * @param tagType the tag identifier
	     * @return the tag's value as a byte array
	     */
	    @Nullable
	    public byte[] getByteArray(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null) {
	            return null;
	        } else if (o instanceof StringValue) {
	            return ((StringValue)o).getBytes();
	        } else if (o instanceof Rational[]) {
	            Rational[] rationals = (Rational[])o;
	            byte[] bytes = new byte[rationals.length];
	            for (int i = 0; i < bytes.length; i++) {
	                bytes[i] = rationals[i].byteValue();
	            }
	            return bytes;
	        } else if (o instanceof byte[]) {
	            return (byte[])o;
	        } else if (o instanceof int[]) {
	            int[] ints = (int[])o;
	            byte[] bytes = new byte[ints.length];
	            for (int i = 0; i < ints.length; i++) {
	                bytes[i] = (byte)ints[i];
	            }
	            return bytes;
	        } else if (o instanceof short[]) {
	            short[] shorts = (short[])o;
	            byte[] bytes = new byte[shorts.length];
	            for (int i = 0; i < shorts.length; i++) {
	                bytes[i] = (byte)shorts[i];
	            }
	            return bytes;
	        } else if (o instanceof CharSequence) {
	            CharSequence str = (CharSequence)o;
	            byte[] bytes = new byte[str.length()];
	            for (int i = 0; i < str.length(); i++) {
	                bytes[i] = (byte)str.charAt(i);
	            }
	            return bytes;
	        }
	        if (o instanceof Integer)
	            return new byte[] { ((Integer)o).byteValue() };

	        return null;
	    }

	    /** Returns the specified tag's value as a double, if possible. */
	    public double getDouble(int tagType) throws MetadataException
	    {
	        Double value = getDoubleObject(tagType);
	        if (value!=null)
	            return value;
	        Object o = getObject(tagType);
	        if (o == null)
	            throw new MetadataException("Tag '" + getTagName(tagType) + "' has not been set -- check using containsTag() first");
	        throw new MetadataException("Tag '" + tagType + "' cannot be converted to a double.  It is of type '" + o.getClass() + "'.");
	    }
	    /** Returns the specified tag's value as a Double.  If the tag is not set or cannot be converted, <code>null</code> is returned. */
	    @Nullable
	    public Double getDoubleObject(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof String || o instanceof StringValue) {
	            try {
	                return Double.parseDouble(o.toString());
	            } catch (NumberFormatException nfe) {
	                return null;
	            }
	        }
	        if (o instanceof Number)
	            return ((Number)o).doubleValue();

	        return null;
	    }

	    /** Returns the specified tag's value as a float, if possible. */
	    public float getFloat(int tagType) throws MetadataException
	    {
	        Float value = getFloatObject(tagType);
	        if (value!=null)
	            return value;
	        Object o = getObject(tagType);
	        if (o == null)
	            throw new MetadataException("Tag '" + getTagName(tagType) + "' has not been set -- check using containsTag() first");
	        throw new MetadataException("Tag '" + tagType + "' cannot be converted to a float.  It is of type '" + o.getClass() + "'.");
	    }

	    /** Returns the specified tag's value as a float.  If the tag is not set or cannot be converted, <code>null</code> is returned. */
	    @Nullable
	    public Float getFloatObject(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof String || o instanceof StringValue) {
	            try {
	                return Float.parseFloat(o.toString());
	            } catch (NumberFormatException nfe) {
	                return null;
	            }
	        }
	        if (o instanceof Number)
	            return ((Number)o).floatValue();
	        return null;
	    }

	    /** Returns the specified tag's value as a long, if possible. */
	    public long getLong(int tagType) throws MetadataException
	    {
	        Long value = getLongObject(tagType);
	        if (value != null)
	            return value;
	        Object o = getObject(tagType);
	        if (o == null)
	            throw new MetadataException("Tag '" + getTagName(tagType) + "' has not been set -- check using containsTag() first");
	        throw new MetadataException("Tag '" + tagType + "' cannot be converted to a long.  It is of type '" + o.getClass() + "'.");
	    }

	    /** Returns the specified tag's value as a long.  If the tag is not set or cannot be converted, <code>null</code> is returned. */
	    @Nullable
	    public Long getLongObject(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof Number)
	            return ((Number)o).longValue();
	        if (o instanceof String || o instanceof StringValue) {
	            try {
	                return Long.parseLong(o.toString());
	            } catch (NumberFormatException nfe) {
	                return null;
	            }
	        } else if (o instanceof Rational[]) {
	            Rational[] rationals = (Rational[])o;
	            if (rationals.length == 1)
	                return rationals[0].longValue();
	        } else if (o instanceof byte[]) {
	            byte[] bytes = (byte[])o;
	            if (bytes.length == 1)
	                return (long)bytes[0];
	        } else if (o instanceof int[]) {
	            int[] ints = (int[])o;
	            if (ints.length == 1)
	                return (long)ints[0];
	        } else if (o instanceof short[]) {
	            short[] shorts = (short[])o;
	            if (shorts.length == 1)
	                return (long)shorts[0];
	        }
	        return null;
	    }

	    /** Returns the specified tag's value as a boolean, if possible. */
	    public boolean getBoolean(int tagType) throws MetadataException
	    {
	        Boolean value = getBooleanObject(tagType);
	        if (value != null)
	            return value;
	        Object o = getObject(tagType);
	        if (o == null)
	            throw new MetadataException("Tag '" + getTagName(tagType) + "' has not been set -- check using containsTag() first");
	        throw new MetadataException("Tag '" + tagType + "' cannot be converted to a boolean.  It is of type '" + o.getClass() + "'.");
	    }

	    /** Returns the specified tag's value as a boolean.  If the tag is not set or cannot be converted, <code>null</code> is returned. */
	    @Nullable
	    @SuppressWarnings(value = "NP_BOOLEAN_RETURN_NULL", justification = "keep API interface consistent")
	    public Boolean getBooleanObject(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;
	        if (o instanceof Boolean)
	            return (Boolean)o;
	        if (o instanceof String || o instanceof StringValue) {
	            try {
	                return Boolean.getBoolean(o.toString());
	            } catch (NumberFormatException nfe) {
	                return null;
	            }
	        }
	        if (o instanceof Number)
	            return (((Number)o).doubleValue() != 0);
	        return null;
	    }

	    /**
	     * Returns the specified tag's value as a java.util.Date.  If the value is unset or cannot be converted, <code>null</code> is returned.
	     * <p>
	     * If the underlying value is a {@link String}, then attempts will be made to parse the string as though it is in
	     * the GMT {@link TimeZone}.  If the {@link TimeZone} is known, call the overload that accepts one as an argument.
	     */
	    @Nullable
	    public java.util.Date getDate(int tagType)
	    {
	        return getDate(tagType, null, null);
	    }

	    /**
	     * Returns the specified tag's value as a java.util.Date.  If the value is unset or cannot be converted, <code>null</code> is returned.
	     * <p>
	     * If the underlying value is a {@link String}, then attempts will be made to parse the string as though it is in
	     * the {@link TimeZone} represented by the {@code timeZone} parameter (if it is non-null).  Note that this parameter
	     * is only considered if the underlying value is a string and it has no time zone information, otherwise it has no effect.
	     */
	    @Nullable
	    public java.util.Date getDate(int tagType, @Nullable TimeZone timeZone)
	    {
	        return getDate(tagType, null, timeZone);
	    }

	    /**
	     * Returns the specified tag's value as a java.util.Date.  If the value is unset or cannot be converted, <code>null</code> is returned.
	     * <p>
	     * If the underlying value is a {@link String}, then attempts will be made to parse the string as though it is in
	     * the {@link TimeZone} represented by the {@code timeZone} parameter (if it is non-null).  Note that this parameter
	     * is only considered if the underlying value is a string and it has no time zone information, otherwise it has no effect.
	     * In addition, the {@code subsecond} parameter, which specifies the number of digits after the decimal point in the seconds,
	     * is set to the returned Date. This parameter is only considered if the underlying value is a string and is has
	     * no subsecond information, otherwise it has no effect.
	     *
	     * @param tagType the tag identifier
	     * @param subsecond the subsecond value for the Date
	     * @param timeZone the time zone to use
	     * @return a Date representing the time value
	     */
	    @Nullable
	    public java.util.Date getDate(int tagType, @Nullable String subsecond, @Nullable TimeZone timeZone)
	    {
	        Object o = getObject(tagType);

	        if (o instanceof java.util.Date)
	            return (java.util.Date)o;

	        java.util.Date date = null;

	        if ((o instanceof String) || (o instanceof StringValue)) {
	            // This seems to cover all known Exif and Xmp date strings
	            // Note that "    :  :     :  :  " is a valid date string according to the Exif spec (which means 'unknown date'): http://www.awaresystems.be/imaging/tiff/tifftags/privateifd/exif/datetimeoriginal.html
	            String datePatterns[] = {
	                    "yyyy:MM:dd HH:mm:ss",
	                    "yyyy:MM:dd HH:mm",
	                    "yyyy-MM-dd HH:mm:ss",
	                    "yyyy-MM-dd HH:mm",
	                    "yyyy.MM.dd HH:mm:ss",
	                    "yyyy.MM.dd HH:mm",
	                    "yyyy-MM-dd'T'HH:mm:ss",
	                    "yyyy-MM-dd'T'HH:mm",
	                    "yyyy-MM-dd",
	                    "yyyy-MM",
	                    "yyyyMMdd", // as used in IPTC data
	                    "yyyy" };

	            String dateString = o.toString();

	            // if the date string has subsecond information, it supersedes the subsecond parameter
	            Pattern subsecondPattern = Pattern.compile("(\\d\\d:\\d\\d:\\d\\d)(\\.\\d+)");
	            Matcher subsecondMatcher = subsecondPattern.matcher(dateString);
	            if (subsecondMatcher.find()) {
	                subsecond = subsecondMatcher.group(2).substring(1);
	                dateString = subsecondMatcher.replaceAll("$1");
	            }

	            // if the date string has time zone information, it supersedes the timeZone parameter
	            Pattern timeZonePattern = Pattern.compile("(Z|[+-]\\d\\d:\\d\\d|[+-]\\d\\d\\d\\d)$");
	            Matcher timeZoneMatcher = timeZonePattern.matcher(dateString);
	            if (timeZoneMatcher.find()) {
	                timeZone = TimeZone.getTimeZone("GMT" + timeZoneMatcher.group().replaceAll("Z", ""));
	                dateString = timeZoneMatcher.replaceAll("");
	            }

	            for (String datePattern : datePatterns) {
	                try {
	                    DateFormat parser = new SimpleDateFormat(datePattern);
	                    if (timeZone != null)
	                        parser.setTimeZone(timeZone);
	                    else
	                        parser.setTimeZone(TimeZone.getTimeZone("GMT")); // don't interpret zone time

	                    date = parser.parse(dateString);
	                    break;
	                } catch (ParseException ex) {
	                    // simply try the next pattern
	                }
	            }
	        }

	        if (date == null)
	            return null;

	        if (subsecond == null)
	            return date;

	        try {
	            int millisecond = (int) (Double.parseDouble("." + subsecond) * 1000);
	            if (millisecond >= 0 && millisecond < 1000) {
	                Calendar calendar = Calendar.getInstance();
	                calendar.setTime(date);
	                calendar.set(Calendar.MILLISECOND, millisecond);
	                return calendar.getTime();
	            }
	            return date;
	        } catch (NumberFormatException e) {
	            return date;
	        }
	    }

	    /** Returns the specified tag's value as a Rational.  If the value is unset or cannot be converted, <code>null</code> is returned. */
	    @Nullable
	    public Rational getRational(int tagType)
	    {
	        Object o = getObject(tagType);

	        if (o == null)
	            return null;

	        if (o instanceof Rational)
	            return (Rational)o;
	        if (o instanceof Integer)
	            return new Rational((Integer)o, 1);
	        if (o instanceof Long)
	            return new Rational((Long)o, 1);

	        // NOTE not doing conversions for real number types

	        return null;
	    }

	    /** Returns the specified tag's value as an array of Rational.  If the value is unset or cannot be converted, <code>null</code> is returned. */
	    @Nullable
	    public Rational[] getRationalArray(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;

	        if (o instanceof Rational[])
	            return (Rational[])o;

	        return null;
	    }

	    /**
	     * Returns the specified tag's value as a String.  This value is the 'raw' value.  A more presentable decoding
	     * of this value may be obtained from the corresponding Descriptor.
	     *
	     * @return the String representation of the tag's value, or
	     *         <code>null</code> if the tag hasn't been defined.
	     */
	    @Nullable
	    public String getString(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o == null)
	            return null;

	        if (o instanceof Rational)
	            return ((Rational)o).toSimpleString(true);

	        if (o.getClass().isArray()) {
	            // handle arrays of objects and primitives
	            int arrayLength = Array.getLength(o);
	            final Class<?> componentType = o.getClass().getComponentType();

	            StringBuilder string = new StringBuilder();

	            if (Object.class.isAssignableFrom(componentType)) {
	                // object array
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    string.append(Array.get(o, i).toString());
	                }
	            } else if (componentType.getName().equals("int")) {
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    string.append(Array.getInt(o, i));
	                }
	            } else if (componentType.getName().equals("short")) {
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    string.append(Array.getShort(o, i));
	                }
	            } else if (componentType.getName().equals("long")) {
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    string.append(Array.getLong(o, i));
	                }
	            } else if (componentType.getName().equals("float")) {
	                DecimalFormat format = new DecimalFormat(_floatFormatPattern);
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    String s = format.format(Array.getFloat(o, i));
	                    string.append(s.equals("-0") ? "0" : s);
	                }
	            } else if (componentType.getName().equals("double")) {
	                DecimalFormat format = new DecimalFormat(_floatFormatPattern);
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    String s = format.format(Array.getDouble(o, i));
	                    string.append(s.equals("-0") ? "0" : s);
	                }
	            } else if (componentType.getName().equals("byte")) {
	                for (int i = 0; i < arrayLength; i++) {
	                    if (i != 0)
	                        string.append(' ');
	                    string.append(Array.getByte(o, i) & 0xff);
	                }
	            } else {
	                addError("Unexpected array component type: " + componentType.getName());
	            }

	            return string.toString();
	        }

	        if (o instanceof Double)
	            return new DecimalFormat(_floatFormatPattern).format(((Double)o).doubleValue());

	        if (o instanceof Float)
	            return new DecimalFormat(_floatFormatPattern).format(((Float)o).floatValue());

	        // Note that several cameras leave trailing spaces (Olympus, Nikon) but this library is intended to show
	        // the actual data within the file.  It is not inconceivable that whitespace may be significant here, so we
	        // do not trim.  Also, if support is added for writing data back to files, this may cause issues.
	        // We leave trimming to the presentation layer.
	        return o.toString();
	    }

	    @Nullable
	    public String getString(int tagType, String charset)
	    {
	        byte[] bytes = getByteArray(tagType);
	        if (bytes==null)
	            return null;
	        try {
	            return new String(bytes, charset);
	        } catch (UnsupportedEncodingException e) {
	            return null;
	        }
	    }

	    @Nullable
	    public StringValue getStringValue(int tagType)
	    {
	        Object o = getObject(tagType);
	        if (o instanceof StringValue)
	            return (StringValue)o;
	        return null;
	    }

	    /**
	     * Returns the object hashed for the particular tag type specified, if available.
	     *
	     * @param tagType the tag type identifier
	     * @return the tag's value as an Object if available, else <code>null</code>
	     */
	    @java.lang.SuppressWarnings({ "UnnecessaryBoxing" })
	    @Nullable
	    public Object getObject(int tagType)
	    {
	        return _tagMap.get(Integer.valueOf(tagType));
	    }

	// OTHER METHODS

	    /**
	     * Returns the name of a specified tag as a String.
	     *
	     * @param tagType the tag type identifier
	     * @return the tag's name as a String
	     */
	    @NotNull
	    public String getTagName(int tagType)
	    {
	        HashMap<Integer, String> nameMap = getTagNameMap();
	        if (!nameMap.containsKey(tagType)) {
	            String hex = Integer.toHexString(tagType);
	            while (hex.length() < 4) {
	                hex = "0" + hex;
	            }
	            return "Unknown tag (0x" + hex + ")";
	        }
	        return nameMap.get(tagType);
	    }

	    /**
	     * Gets whether the specified tag is known by the directory and has a name.
	     *
	     * @param tagType the tag type identifier
	     * @return whether this directory has a name for the specified tag
	     */
	    public boolean hasTagName(int tagType)
	    {
	        return getTagNameMap().containsKey(tagType);
	    }

	    /**
	     * Provides a description of a tag's value using the descriptor set by
	     * <code>setDescriptor(Descriptor)</code>.
	     *
	     * @param tagType the tag type identifier
	     * @return the tag value's description as a String
	     */
	    @Nullable
	    public String getDescription(int tagType)
	    {
	        assert(_descriptor != null);
	        return _descriptor.getDescription(tagType);
	    }

	    @Override
	    public String toString()
	    {
	        return String.format("%s Directory (%d %s)",
	            getName(),
	            _tagMap.size(),
	            _tagMap.size() == 1
	                ? "tag"
	                : "tags");
	    }
	}



	/**
	 * A top-level object that holds the metadata values extracted from an image.
	 * <p>
	 * Metadata objects may contain zero or more {@link Directory} objects.  Each directory may contain zero or more tags
	 * with corresponding values.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public final class Metadata
	{
	    /**
	     * The list of {@link Directory} instances in this container, in the order they were added.
	     */
	    @NotNull
	    private final List<Directory> _directories = new ArrayList<Directory>();

	    /**
	     * Returns an iterable set of the {@link Directory} instances contained in this metadata collection.
	     *
	     * @return an iterable set of directories
	     */
	    @NotNull
	    public Iterable<Directory> getDirectories()
	    {
	        return _directories;
	    }

	    @NotNull
	    //@SuppressWarnings("unchecked")
	    public <T extends Directory> Collection<T> getDirectoriesOfType(Class<T> type)
	    {
	        List<T> directories = new ArrayList<T>();
	        for (Directory dir : _directories) {
	            if (type.isAssignableFrom(dir.getClass())) {
	                directories.add((T)dir);
	            }
	        }
	        return directories;
	    }

	    /**
	     * Returns the count of directories in this metadata collection.
	     *
	     * @return the number of unique directory types set for this metadata collection
	     */
	    public int getDirectoryCount()
	    {
	        return _directories.size();
	    }

	    /**
	     * Adds a directory to this metadata collection.
	     *
	     * @param directory the {@link Directory} to add into this metadata collection.
	     */
	    public <T extends Directory> void addDirectory(@NotNull T directory)
	    {
	        if (directory == null) {
	            throw new IllegalArgumentException("Directory may not be null.");
	        }

	        _directories.add(directory);
	    }

	    /**
	     * Gets the first {@link Directory} of the specified type contained within this metadata collection.
	     * If no instances of this type are present, <code>null</code> is returned.
	     *
	     * @param type the Directory type
	     * @param <T> the Directory type
	     * @return the first Directory of type T in this metadata collection, or <code>null</code> if none exist
	     */
	    @Nullable
	    //@SuppressWarnings("unchecked")
	    public <T extends Directory> T getFirstDirectoryOfType(@NotNull Class<T> type)
	    {
	        for (Directory dir : _directories) {
	            if (type.isAssignableFrom(dir.getClass()))
	                return (T)dir;
	        }
	        return null;
	    }

	    /**
	     * Indicates whether an instance of the given directory type exists in this Metadata instance.
	     *
	     * @param type the {@link Directory} type
	     * @return <code>true</code> if a {@link Directory} of the specified type exists, otherwise <code>false</code>
	     */
	    public boolean containsDirectoryOfType(Class<? extends Directory> type)
	    {
	        for (Directory dir : _directories) {
	            if (type.isAssignableFrom(dir.getClass()))
	                return true;
	        }
	        return false;
	    }

	    /**
	     * Indicates whether any errors were reported during the reading of metadata values.
	     * This value will be true if Directory.hasErrors() is true for one of the contained {@link Directory} objects.
	     *
	     * @return whether one of the contained directories has an error
	     */
	    public boolean hasErrors()
	    {
	        for (Directory directory : getDirectories()) {
	            if (directory.hasErrors())
	                return true;
	        }
	        return false;
	    }

	    @Override
	    public String toString()
	    {
	        int count = getDirectoryCount();
	        return String.format("Metadata (%d %s)",
	            count,
	            count == 1
	                ? "directory"
	                : "directories");
	    }
	}
	
	/**
	 * An enumeration of the known segment types found in JPEG files.
	 *
	 * <ul>
	 *     <li>http://www.ozhiker.com/electronics/pjmt/jpeg_info/app_segments.html</li>
	 *     <li>http://www.sno.phy.queensu.ca/~phil/exiftool/TagNames/JPEG.html</li>
	 * </ul>
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public enum JpegSegmentType
	{
	    /** APP0 JPEG segment identifier. Commonly contains JFIF, JFXX. */
	    APP0((byte)0xE0, true),

	    /** APP1 JPEG segment identifier. Commonly contains Exif. XMP data is also kept in here, though usually in a second instance. */
	    APP1((byte)0xE1, true),

	        /** APP2 JPEG segment identifier. Commonly contains ICC. */
	    APP2((byte)0xE2, true),

	    /** APP3 JPEG segment identifier. */
	    APP3((byte)0xE3, true),

	    /** APP4 JPEG segment identifier. */
	    APP4((byte)0xE4, true),

	    /** APP5 JPEG segment identifier. */
	    APP5((byte)0xE5, true),

	    /** APP6 JPEG segment identifier. */
	    APP6((byte)0xE6, true),

	    /** APP7 JPEG segment identifier. */
	    APP7((byte)0xE7, true),

	    /** APP8 JPEG segment identifier. */
	    APP8((byte)0xE8, true),

	    /** APP9 JPEG segment identifier. */
	    APP9((byte)0xE9, true),

	    /** APPA (App10) JPEG segment identifier. Can contain Unicode comments, though {@link JpegSegmentType#COM} is more commonly used for comments. */
	    APPA((byte)0xEA, true),

	    /** APPB (App11) JPEG segment identifier. */
	    APPB((byte)0xEB, true),

	    /** APPC (App12) JPEG segment identifier. */
	    APPC((byte)0xEC, true),

	    /** APPD (App13) JPEG segment identifier. Commonly contains IPTC, Photoshop data. */
	    APPD((byte)0xED, true),

	    /** APPE (App14) JPEG segment identifier. Commonly contains Adobe data. */
	    APPE((byte)0xEE, true),

	    /** APPF (App15) JPEG segment identifier. */
	    APPF((byte)0xEF, true),

	    /** Start Of Image segment identifier. */
	    SOI((byte)0xD8, false),

	    /** Define Quantization Table segment identifier. */
	    DQT((byte)0xDB, false),

	    /** Define Number of Lines segment identifier. */
	    DNL((byte)0xDC, false),

	    /** Define Restart Interval segment identifier. */
	    DRI((byte)0xDD, false),

	    /** Define Hierarchical Progression segment identifier. */
	    DHP((byte)0xDE, false),

	    /** EXPand reference component(s) segment identifier. */
	    EXP((byte)0xDF, false),

	    /** Define Huffman Table segment identifier. */
	    DHT((byte)0xC4, false),

	    /** Define Arithmetic Coding conditioning segment identifier. */
	    DAC((byte)0xCC, false),

	    /** Start-of-Frame (0) segment identifier for Baseline DCT. */
	    SOF0((byte)0xC0, true),

	    /** Start-of-Frame (1) segment identifier for Extended sequential DCT. */
	    SOF1((byte)0xC1, true),

	    /** Start-of-Frame (2) segment identifier for Progressive DCT. */
	    SOF2((byte)0xC2, true),

	    /** Start-of-Frame (3) segment identifier for Lossless (sequential). */
	    SOF3((byte)0xC3, true),

//	    /** Start-of-Frame (4) segment identifier. */
//	    SOF4((byte)0xC4, true),

	    /** Start-of-Frame (5) segment identifier for Differential sequential DCT. */
	    SOF5((byte)0xC5, true),

	    /** Start-of-Frame (6) segment identifier for Differential progressive DCT. */
	    SOF6((byte)0xC6, true),

	    /** Start-of-Frame (7) segment identifier for Differential lossless (sequential). */
	    SOF7((byte)0xC7, true),

	    /** Reserved for JPEG extensions. */
	    JPG((byte)0xC8, true),

	    /** Start-of-Frame (9) segment identifier for Extended sequential DCT. */
	    SOF9((byte)0xC9, true),

	    /** Start-of-Frame (10) segment identifier for Progressive DCT. */
	    SOF10((byte)0xCA, true),

	    /** Start-of-Frame (11) segment identifier for Lossless (sequential). */
	    SOF11((byte)0xCB, true),

//	    /** Start-of-Frame (12) segment identifier. */
//	    SOF12((byte)0xCC, true),

	    /** Start-of-Frame (13) segment identifier for Differential sequential DCT. */
	    SOF13((byte)0xCD, true),

	    /** Start-of-Frame (14) segment identifier for Differential progressive DCT. */
	    SOF14((byte)0xCE, true),

	    /** Start-of-Frame (15) segment identifier for Differential lossless (sequential). */
	    SOF15((byte)0xCF, true),

	    /** JPEG comment segment identifier for comments. */
	    COM((byte)0xFE, true);

	    public static final Collection<JpegSegmentType> canContainMetadataTypes;

	    static {
	        List<JpegSegmentType> segmentTypes = new ArrayList<JpegSegmentType>();
	        for (JpegSegmentType segmentType : JpegSegmentType.class.getEnumConstants()) {
	            if (segmentType.canContainMetadata) {
	                segmentTypes.add(segmentType);
	            }
	        }
	        canContainMetadataTypes = segmentTypes;
	    }

	    public final byte byteValue;
	    public final boolean canContainMetadata;

	    JpegSegmentType(byte byteValue, boolean canContainMetadata)
	    {
	        this.byteValue = byteValue;
	        this.canContainMetadata = canContainMetadata;
	    }

	    @Nullable
	    public static JpegSegmentType fromByte(byte segmentTypeByte)
	    {
	        for (JpegSegmentType segmentType : JpegSegmentType.class.getEnumConstants()) {
	            if (segmentType.byteValue == segmentTypeByte)
	                return segmentType;
	        }
	        return null;
	    }
	}

	
	/**
	 * Defines an object that extracts metadata from in JPEG segments.
	 */
	public interface JpegSegmentMetadataReader
	{
	    /**
	     * Gets the set of JPEG segment types that this reader is interested in.
	     */
	    @NotNull
	    Iterable<JpegSegmentType> getSegmentTypes();

	    /**
	     * Extracts metadata from all instances of a particular JPEG segment type.
	     *
	     * @param segments A sequence of byte arrays from which the metadata should be extracted. These are in the order
	     *                 encountered in the original file.
	     * @param metadata The {@link Metadata} object into which extracted values should be merged.
	     * @param segmentType The {@link JpegSegmentType} being read.
	     */
	    void readJpegSegments(@NotNull final Iterable<byte[]> segments, @NotNull final Metadata metadata, @NotNull final JpegSegmentType segmentType);
	}
	
	/**
	 * Holds a collection of JPEG data segments.  This need not necessarily be all segments
	 * within the JPEG. For example, it may be convenient to store only the non-image
	 * segments when analysing metadata.
	 * <p>
	 * Segments are keyed via their {@link JpegSegmentType}. Where multiple segments use the
	 * same segment type, they will all be stored and available.
	 * <p>
	 * Each segment type may contain multiple entries. Conceptually the model is:
	 * <code>Map&lt;JpegSegmentType, Collection&lt;byte[]&gt;&gt;</code>. This class provides
	 * convenience methods around that structure.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class JpegSegmentData
	{
	    // TODO key this on JpegSegmentType rather than Byte, and hopefully lose much of the use of 'byte' with this class
	    @NotNull
	    private final HashMap<Byte, List<byte[]>> _segmentDataMap = new HashMap<Byte, List<byte[]>>(10);

	    /**
	     * Adds segment bytes to the collection.
	     *
	     * @param segmentType  the type of the segment being added
	     * @param segmentBytes the byte array holding data for the segment being added
	     */
	    //@SuppressWarnings({"MismatchedQueryAndUpdateOfCollection"})
	    public void addSegment(byte segmentType, @NotNull byte[] segmentBytes)
	    {
	        getOrCreateSegmentList(segmentType).add(segmentBytes);
	    }

	    /**
	     * Gets the set of JPEG segment type identifiers.
	     */
	    public Iterable<JpegSegmentType> getSegmentTypes()
	    {
	        Set<JpegSegmentType> segmentTypes = new HashSet<JpegSegmentType>();

	        for (Byte segmentTypeByte : _segmentDataMap.keySet())
	        {
	            JpegSegmentType segmentType = JpegSegmentType.fromByte(segmentTypeByte);
	            if (segmentType == null) {
	                throw new IllegalStateException("Should not have a segmentTypeByte that is not in the enum: " + Integer.toHexString(segmentTypeByte));
	            }
	            segmentTypes.add(segmentType);
	        }

	        return segmentTypes;
	    }

	    /**
	     * Gets the first JPEG segment data for the specified type.
	     *
	     * @param segmentType the JpegSegmentType for the desired segment
	     * @return a byte[] containing segment data or null if no data exists for that segment
	     */
	    @Nullable
	    public byte[] getSegment(byte segmentType)
	    {
	        return getSegment(segmentType, 0);
	    }

	    /**
	     * Gets the first JPEG segment data for the specified type.
	     *
	     * @param segmentType the JpegSegmentType for the desired segment
	     * @return a byte[] containing segment data or null if no data exists for that segment
	     */
	    @Nullable
	    public byte[] getSegment(@NotNull JpegSegmentType segmentType)
	    {
	        return getSegment(segmentType.byteValue, 0);
	    }

	    /**
	     * Gets segment data for a specific occurrence and type.  Use this method when more than one occurrence
	     * of segment data for a given type exists.
	     *
	     * @param segmentType identifies the required segment
	     * @param occurrence  the zero-based index of the occurrence
	     * @return the segment data as a byte[], or null if no segment exists for the type &amp; occurrence
	     */
	    @Nullable
	    public byte[] getSegment(@NotNull JpegSegmentType segmentType, int occurrence)
	    {
	        return getSegment(segmentType.byteValue, occurrence);
	    }

	    /**
	     * Gets segment data for a specific occurrence and type.  Use this method when more than one occurrence
	     * of segment data for a given type exists.
	     *
	     * @param segmentType identifies the required segment
	     * @param occurrence  the zero-based index of the occurrence
	     * @return the segment data as a byte[], or null if no segment exists for the type &amp; occurrence
	     */
	    @Nullable
	    public byte[] getSegment(byte segmentType, int occurrence)
	    {
	        final List<byte[]> segmentList = getSegmentList(segmentType);

	        return segmentList != null && segmentList.size() > occurrence
	                ? segmentList.get(occurrence)
	                : null;
	    }

	    /**
	     * Returns all instances of a given JPEG segment.  If no instances exist, an empty sequence is returned.
	     *
	     * @param segmentType a number which identifies the type of JPEG segment being queried
	     * @return zero or more byte arrays, each holding the data of a JPEG segment
	     */
	    @NotNull
	    public Iterable<byte[]> getSegments(@NotNull JpegSegmentType segmentType)
	    {
	        return getSegments(segmentType.byteValue);
	    }

	    /**
	     * Returns all instances of a given JPEG segment.  If no instances exist, an empty sequence is returned.
	     *
	     * @param segmentType a number which identifies the type of JPEG segment being queried
	     * @return zero or more byte arrays, each holding the data of a JPEG segment
	     */
	    @NotNull
	    public Iterable<byte[]> getSegments(byte segmentType)
	    {
	        final List<byte[]> segmentList = getSegmentList(segmentType);
	        return segmentList == null ? new ArrayList<byte[]>() : segmentList;
	    }

	    @Nullable
	    private List<byte[]> getSegmentList(byte segmentType)
	    {
	        return _segmentDataMap.get(segmentType);
	    }

	    @NotNull
	    private List<byte[]> getOrCreateSegmentList(byte segmentType)
	    {
	        List<byte[]> segmentList;
	        if (_segmentDataMap.containsKey(segmentType)) {
	            segmentList = _segmentDataMap.get(segmentType);
	        } else {
	            segmentList = new ArrayList<byte[]>();
	            _segmentDataMap.put(segmentType, segmentList);
	        }
	        return segmentList;
	    }

	    /**
	     * Returns the count of segment data byte arrays stored for a given segment type.
	     *
	     * @param segmentType identifies the required segment
	     * @return the segment count (zero if no segments exist).
	     */
	    public int getSegmentCount(@NotNull JpegSegmentType segmentType)
	    {
	        return getSegmentCount(segmentType.byteValue);
	    }

	    /**
	     * Returns the count of segment data byte arrays stored for a given segment type.
	     *
	     * @param segmentType identifies the required segment
	     * @return the segment count (zero if no segments exist).
	     */
	    public int getSegmentCount(byte segmentType)
	    {
	        final List<byte[]> segmentList = getSegmentList(segmentType);
	        return segmentList == null ? 0 : segmentList.size();
	    }

	    /**
	     * Removes a specified instance of a segment's data from the collection.  Use this method when more than one
	     * occurrence of segment data exists for a given type exists.
	     *
	     * @param segmentType identifies the required segment
	     * @param occurrence  the zero-based index of the segment occurrence to remove.
	     */
	    //@SuppressWarnings({"MismatchedQueryAndUpdateOfCollection"})
	    public void removeSegmentOccurrence(@NotNull JpegSegmentType segmentType, int occurrence)
	    {
	        removeSegmentOccurrence(segmentType.byteValue, occurrence);
	    }

	    /**
	     * Removes a specified instance of a segment's data from the collection.  Use this method when more than one
	     * occurrence of segment data exists for a given type exists.
	     *
	     * @param segmentType identifies the required segment
	     * @param occurrence  the zero-based index of the segment occurrence to remove.
	     */
	    //@SuppressWarnings({"MismatchedQueryAndUpdateOfCollection"})
	    public void removeSegmentOccurrence(byte segmentType, int occurrence)
	    {
	        final List<byte[]> segmentList = _segmentDataMap.get(segmentType);
	        segmentList.remove(occurrence);
	    }

	    /**
	     * Removes all segments from the collection having the specified type.
	     *
	     * @param segmentType identifies the required segment
	     */
	    public void removeSegment(@NotNull JpegSegmentType segmentType)
	    {
	        removeSegment(segmentType.byteValue);
	    }

	    /**
	     * Removes all segments from the collection having the specified type.
	     *
	     * @param segmentType identifies the required segment
	     */
	    public void removeSegment(byte segmentType)
	    {
	        _segmentDataMap.remove(segmentType);
	    }

	    /**
	     * Determines whether data is present for a given segment type.
	     *
	     * @param segmentType identifies the required segment
	     * @return true if data exists, otherwise false
	     */
	    public boolean containsSegment(@NotNull JpegSegmentType segmentType)
	    {
	        return containsSegment(segmentType.byteValue);
	    }

	    /**
	     * Determines whether data is present for a given segment type.
	     *
	     * @param segmentType identifies the required segment
	     * @return true if data exists, otherwise false
	     */
	    public boolean containsSegment(byte segmentType)
	    {
	        return _segmentDataMap.containsKey(segmentType);
	    }
	}
	
	/**
	 * An exception class thrown upon an unexpected condition that was fatal for the processing of an image.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ImageProcessingException extends CompoundException
	{
	    private static final long serialVersionUID = -9115669182209912676L;

	    public ImageProcessingException(@Nullable String message)
	    {
	        super(message);
	    }

	    public ImageProcessingException(@Nullable String message, @Nullable Throwable cause)
	    {
	        super(message, cause);
	    }

	    public ImageProcessingException(@Nullable Throwable cause)
	    {
	        super(cause);
	    }
	}

	/**
	 * An exception class thrown upon unexpected and fatal conditions while processing a JPEG file.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class JpegProcessingException extends ImageProcessingException
	{
	    private static final long serialVersionUID = -7870179776125450158L;

	    public JpegProcessingException(@Nullable String message)
	    {
	        super(message);
	    }

	    public JpegProcessingException(@Nullable String message, @Nullable Throwable cause)
	    {
	        super(message, cause);
	    }

	    public JpegProcessingException(@Nullable Throwable cause)
	    {
	        super(cause);
	    }
	}

	
	/**
	 * Performs read functions of JPEG files, returning specific file segments.
	 * <p>
	 * JPEG files are composed of a sequence of consecutive JPEG 'segments'. Each is identified by one of a set of byte
	 * values, modelled in the {@link JpegSegmentType} enumeration. Use <code>readSegments</code> to read out the some
	 * or all segments into a {@link JpegSegmentData} object, from which the raw JPEG segment byte arrays may be accessed.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public static class JpegSegmentReader
	{
	    /**
	     * The 0xFF byte that signals the start of a segment.
	     */
	    private static final byte SEGMENT_IDENTIFIER = (byte) 0xFF;

	    /**
	     * Private, because this segment crashes my algorithm, and searching for it doesn't work (yet).
	     */
	    private static final byte SEGMENT_SOS = (byte) 0xDA;

	    /**
	     * Private, because one wouldn't search for it.
	     */
	    private static final byte MARKER_EOI = (byte) 0xD9;

	    /**
	     * Processes the provided JPEG data, and extracts the specified JPEG segments into a {@link JpegSegmentData} object.
	     * <p>
	     * Will not return SOS (start of scan) or EOI (end of image) segments.
	     *
	     * @param file a {@link File} from which the JPEG data will be read.
	     * @param segmentTypes the set of JPEG segments types that are to be returned. If this argument is <code>null</code>
	     *                     then all found segment types are returned.
	     */
	    @NotNull
	    public static JpegSegmentData readSegments(@NotNull File file, @Nullable Iterable<JpegSegmentType> segmentTypes) throws JpegProcessingException, IOException
	    {
	        FileInputStream stream = null;
	        try {
	            stream = new FileInputStream(file);
	            MetadataExtractor me = new MetadataExtractor();
	            return readSegments(me.new StreamReader(stream), segmentTypes);
	        } finally {
	            if (stream != null) {
	                stream.close();
	            }
	        }
	    }

	    /**
	     * Processes the provided JPEG data, and extracts the specified JPEG segments into a {@link JpegSegmentData} object.
	     * <p>
	     * Will not return SOS (start of scan) or EOI (end of image) segments.
	     *
	     * @param reader a {@link SequentialReader} from which the JPEG data will be read. It must be positioned at the
	     *               beginning of the JPEG data stream.
	     * @param segmentTypes the set of JPEG segments types that are to be returned. If this argument is <code>null</code>
	     *                     then all found segment types are returned.
	     */
	    @NotNull
	    public static JpegSegmentData readSegments(@NotNull final SequentialReader reader, @Nullable Iterable<JpegSegmentType> segmentTypes) throws JpegProcessingException, IOException
	    {
	        // Must be big-endian
	        assert (reader.isMotorolaByteOrder());

	        // first two bytes should be JPEG magic number
	        final int magicNumber = reader.getUInt16();
	        if (magicNumber != 0xFFD8) {
	        	MetadataExtractor me = new MetadataExtractor();
	            throw me.new JpegProcessingException("JPEG data is expected to begin with 0xFFD8 (Ã¿Ã˜) not 0x" + Integer.toHexString(magicNumber));
	        }

	        Set<Byte> segmentTypeBytes = null;
	        if (segmentTypes != null) {
	            segmentTypeBytes = new HashSet<Byte>();
	            for (JpegSegmentType segmentType : segmentTypes) {
	                segmentTypeBytes.add(segmentType.byteValue);
	            }
	        }

        	MetadataExtractor me = new MetadataExtractor();
	        JpegSegmentData segmentData = me.new JpegSegmentData();

	        do {
	            // Find the segment marker. Markers are zero or more 0xFF bytes, followed
	            // by a 0xFF and then a byte not equal to 0x00 or 0xFF.

	            byte segmentIdentifier = reader.getInt8();
	            byte segmentType = reader.getInt8();

	            // Read until we have a 0xFF byte followed by a byte that is not 0xFF or 0x00
	            while (segmentIdentifier != SEGMENT_IDENTIFIER || segmentType == SEGMENT_IDENTIFIER || segmentType == 0) {
	            	segmentIdentifier = segmentType;
	            	segmentType = reader.getInt8();
	            }

	            if (segmentType == SEGMENT_SOS) {
	                // The 'Start-Of-Scan' segment's length doesn't include the image data, instead would
	                // have to search for the two bytes: 0xFF 0xD9 (EOI).
	                // It comes last so simply return at this point
	                return segmentData;
	            }

	            if (segmentType == MARKER_EOI) {
	                // the 'End-Of-Image' segment -- this should never be found in this fashion
	                return segmentData;
	            }

	            // next 2-bytes are <segment-size>: [high-byte] [low-byte]
	            int segmentLength = reader.getUInt16();

	            // segment length includes size bytes, so subtract two
	            segmentLength -= 2;

	            if (segmentLength < 0) {
	                throw me.new JpegProcessingException("JPEG segment size would be less than zero");
	            }

	            // Check whether we are interested in this segment
	            if (segmentTypeBytes == null || segmentTypeBytes.contains(segmentType)) {
	                byte[] segmentBytes = reader.getBytes(segmentLength);
	                assert (segmentLength == segmentBytes.length);
	                segmentData.addSegment(segmentType, segmentBytes);
	            } else {
	                // Skip this segment
	                if (!reader.trySkip(segmentLength)) {
	                    // If skipping failed, just return the segments we found so far
	                    return segmentData;
	                }
	            }

	        } while (true);
	    }

	    private JpegSegmentReader() throws Exception
	    {
	        throw new Exception("Not intended for instantiation.");
	    }
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	//@SuppressWarnings("WeakerAccess")
	public class FileSystemDescriptor extends TagDescriptor<FileSystemDirectory>
	{
		public static final int TAG_FILE_SIZE = 2;
	    public FileSystemDescriptor(@NotNull FileSystemDirectory directory)
	    {
	        super(directory);
	    }

	    @Override
	    @Nullable
	    public String getDescription(int tagType)
	    {
	        switch (tagType) {
	            case TAG_FILE_SIZE:
	                return getFileSizeDescription();
	            default:
	                return super.getDescription(tagType);
	        }
	    }

	    @Nullable
	    private String getFileSizeDescription()
	    {
	        Long size = _directory.getLongObject(TAG_FILE_SIZE);

	        if (size == null)
	            return null;

	        return Long.toString(size) + " bytes";
	    }
	}


	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	//@SuppressWarnings("WeakerAccess")
	public class FileSystemDirectory extends Directory
	{
	    public static final int TAG_FILE_NAME = 1;
	    public static final int TAG_FILE_SIZE = 2;
	    public static final int TAG_FILE_MODIFIED_DATE = 3;

	    @NotNull
	    private final HashMap<Integer, String> _tagNameMap = new HashMap<Integer, String>();

	     {
	        _tagNameMap.put(TAG_FILE_NAME, "File Name");
	        _tagNameMap.put(TAG_FILE_SIZE, "File Size");
	        _tagNameMap.put(TAG_FILE_MODIFIED_DATE, "File Modified Date");
	    }

	    public FileSystemDirectory()
	    {
	        this.setDescriptor(new FileSystemDescriptor(this));
	    }

	    @Override
	    @NotNull
	    public String getName()
	    {
	        return "File";
	    }

	    @Override
	    @NotNull
	    protected HashMap<Integer, String> getTagNameMap()
	    {
	        return _tagNameMap;
	    }
	}


	public class FileSystemMetadataReader
	{
	    public void read(@NotNull File file, @NotNull Metadata metadata) throws IOException
	    {
	        if (!file.isFile())
	            throw new IOException("File object must reference a file");
	        if (!file.exists())
	            throw new IOException("File does not exist");
	        if (!file.canRead())
	            throw new IOException("File is not readable");

	        FileSystemDirectory directory = metadata.getFirstDirectoryOfType(FileSystemDirectory.class);

	        if (directory == null) {
	            directory = new FileSystemDirectory();
	            metadata.addDirectory(directory);
	        }

	        directory.setString(FileSystemDirectory.TAG_FILE_NAME, file.getName());
	        directory.setLong(FileSystemDirectory.TAG_FILE_SIZE, file.length());
	        directory.setDate(FileSystemDirectory.TAG_FILE_MODIFIED_DATE, new Date(file.lastModified()));
	    }
	}


	/**
	 * Obtains all available metadata from JPEG formatted files.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public static class JpegMetadataReader
	{
	    public static final Iterable<JpegSegmentMetadataReader> ALL_READERS = Arrays.asList(
	    		/*
	            new JpegReader(),
	            new JpegCommentReader(),
	            new JfifReader(),
	            new JfxxReader(),
	            new ExifReader(),
	            new XmpReader(),
	            new IccReader(),
	            new PhotoshopReader(),
	            new DuckyReader(),
	            new IptcReader(),
	            new AdobeJpegReader(),
	            new JpegDhtReader(),
	            new JpegDnlReader()
	            */
	    );

	    @NotNull
	    public static Metadata readMetadata(@NotNull InputStream inputStream, @Nullable Iterable<JpegSegmentMetadataReader> readers) throws JpegProcessingException, IOException
	    {
	    	MetadataExtractor me = new MetadataExtractor();
	        Metadata metadata = me.new Metadata();
	        process(metadata, inputStream, readers);
	        return metadata;
	    }

	    @NotNull
	    public static Metadata readMetadata(@NotNull InputStream inputStream) throws JpegProcessingException, IOException
	    {
	        return readMetadata(inputStream, null);
	    }

	    @NotNull
	    public static Metadata readMetadata(@NotNull File file, @Nullable Iterable<JpegSegmentMetadataReader> readers) throws JpegProcessingException, IOException
	    {
	        InputStream inputStream = new FileInputStream(file);
	        Metadata metadata;
	        try {
	            metadata = readMetadata(inputStream, readers);
	        } finally {
	            inputStream.close();
	        }
	        MetadataExtractor me = new MetadataExtractor();
	        me.new FileSystemMetadataReader().read(file, metadata);
	        return metadata;
	    }

	    @NotNull
	    public static Metadata readMetadata(@NotNull File file) throws JpegProcessingException, IOException
	    {
	        return readMetadata(file, null);
	    }

	    public static void process(@NotNull Metadata metadata, @NotNull InputStream inputStream) throws JpegProcessingException, IOException
	    {
	        process(metadata, inputStream, null);
	    }

	    public static void process(@NotNull Metadata metadata, @NotNull InputStream inputStream, @Nullable Iterable<JpegSegmentMetadataReader> readers) throws JpegProcessingException, IOException
	    {
	        if (readers == null)
	            readers = ALL_READERS;

	        Set<JpegSegmentType> segmentTypes = new HashSet<JpegSegmentType>();
	        for (JpegSegmentMetadataReader reader : readers) {
	            for (JpegSegmentType type : reader.getSegmentTypes()) {
	                segmentTypes.add(type);
	            }
	        }

	        MetadataExtractor me = new MetadataExtractor();
	        JpegSegmentData segmentData = JpegSegmentReader.readSegments(me.new StreamReader(inputStream), segmentTypes);

	        processJpegSegmentData(metadata, readers, segmentData);
	    }

	    public static void processJpegSegmentData(Metadata metadata, Iterable<JpegSegmentMetadataReader> readers, JpegSegmentData segmentData)
	    {
	        // Pass the appropriate byte arrays to each reader.
	        for (JpegSegmentMetadataReader reader : readers) {
	            for (JpegSegmentType segmentType : reader.getSegmentTypes()) {
	                reader.readJpegSegments(segmentData.getSegments(segmentType), metadata, segmentType);
	            }
	        }
	    }

	    private JpegMetadataReader() throws Exception
	    {
	        throw new Exception("Not intended for instantiation");
	    }
	}
	
}