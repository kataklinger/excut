using excut.core;
using ExcelDna.Integration;
using Microsoft.FSharp.Collections;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace excut.wrap {
    public static class Wrapper {
        [ExcelFunction(Description = "Generates cutting list")]
        public static string CUTLST(int binSize, int cutSize, object data) {
            if (cutSize < 0) {
                throw new ArgumentOutOfRangeException("Cut size must be zero or positive number", nameof(cutSize));
            }

            if (binSize <= cutSize) {
                throw new ArgumentOutOfRangeException("Bin size must be greater than cut size", nameof(binSize));
            }

            if (!(data is object[,] table) || table.GetLength(0) < 1 || table.GetLength(1) != 3) {
                throw new ArgumentException("Cutouts table must have at least one row with label, count and length", nameof(data));
            }

            var cutouts = new List<Engine.Cutout>();
            for (int i = 0; i < table.GetLength(0); i++) {
                if (table[i, 1] is double cutoutCount && cutoutCount > 0 &&
                    table[i, 2] is double cutoutSize && cutoutSize > 0 && cutoutSize <= binSize) {
                    for (int j = 0; j < cutoutCount; ++j) {
                        cutouts.Add(new Engine.Cutout(table[i, 0]?.ToString(), (int)cutoutSize));
                    }
                }
            }

            var bins = Engine.optimize(new Engine.Options(binSize, cutSize), ListModule.OfSeq(cutouts));

            var sb = new StringBuilder();
            var no = 0;
            foreach (var bin in bins) {
                sb.Append($"Bin #{++no} ({bin.used}/{bin.cuts}/{bin.rest}): ");
                sb.Append(string.Join(", ", bin.cutouts.Select(c => $"{c.size} ({c.label})")));
                sb.Append(Environment.NewLine);
            }

            return sb.ToString();
        }
    }
}
