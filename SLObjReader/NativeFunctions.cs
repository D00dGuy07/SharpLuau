﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace SLObjReader
{
	[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
	public class OpenFileName
	{
		public int structSize = 0;
		public IntPtr hwnd = IntPtr.Zero;
		public IntPtr hinst = IntPtr.Zero;
		public string? filter = null;
		public string? custFilter = null;
		public int custFilterMax = 0;
		public int filterIndex = 0;
		public string? file = null;
		public int maxFile = 0;
		public string? fileTitle = null;
		public int maxFileTitle = 0;
		public string? initialDir = null;
		public string? title = null;
		public int flags = 0;
		public short fileOffset = 0;
		public short fileExtMax = 0;
		public string? defExt = null;
		public int custData = 0;
		public IntPtr pHook = IntPtr.Zero;
		public string? template = null;
	}

	internal class NativeFunctions
	{
		[DllImport("Comdlg32.dll", CharSet = CharSet.Auto)]
		public static extern bool GetOpenFileName([In, Out] OpenFileName ofn);
	}
}
