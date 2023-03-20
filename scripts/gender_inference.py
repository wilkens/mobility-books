import argparse
import os
import glob
import json
import pandas as pd
from tqdm import tqdm

def readArgs ():
    parser = argparse.ArgumentParser (description="Collects the inferred gender for named entities")
    parser.add_argument ("--booknlp-dirs", required=True, type=str, nargs="+", help="directory path for booknlp data for PG books")
    parser.add_argument ("--output-file", required=True, type=str, help="Flat file contains the inferred gender values")
    args = parser.parse_args ()
    return args

def main (args):
    rows = list ()
    rows.append (["book_id", "char_id", "inf_gender", "prob"])
    for dirname in args.booknlp_dirs:
        for filename in tqdm (glob.glob (os.path.join (dirname, f"*.book"))):
            book_id = os.path.basename (filename).split (".")[0]
            with open (filename) as fin:
                js = json.load (fin)

            for char in js["characters"]:
                if char["g"] is not None:
                    rows.append ([book_id, \
                                  char["id"], \
                                  char["g"]["argmax"], \
                                  char["g"]["max"]])

    df = pd.DataFrame (rows[1:], columns=rows[0])
    df.to_csv (args.output_file, sep="\t", header=True, index=False)

if __name__ == "__main__":
    main (readArgs ())
