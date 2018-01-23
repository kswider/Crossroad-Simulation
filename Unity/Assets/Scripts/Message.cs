using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

public class Message
{
    public ushort Length { get; set; }
    public byte[] Content { get; set; }

    public static Message ReadFromStream(BinaryReader reader)
    {
        ushort len;
        byte[] len_buf;
        byte[] buffer;

        len_buf = reader.ReadBytes(2);
        if (BitConverter.IsLittleEndian)
        {
            Array.Reverse(len_buf);
        }
        len = BitConverter.ToUInt16(len_buf, 0);

        buffer = reader.ReadBytes(len);

        return new Message(buffer);
    }

    public void WriteToStream(BinaryWriter writer)
    {
        byte[] len_bytes = BitConverter.GetBytes(Length);

        if (BitConverter.IsLittleEndian)
        {
            Array.Reverse(len_bytes);
        }
        writer.Write(len_bytes);

        writer.Write(Content);
    }

    public Message(byte[] data)
    {
        Content = data;
    }
}