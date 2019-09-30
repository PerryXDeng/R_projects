import pandas as pd
import glob

def index_epoch_and_offset_time():
  files = glob.glob("./*/*.csv")
  for fil in files:
    df = pd.read_csv(fil, index_col=False, header=0)
    df = df[0:101]
    df = df.drop(columns=['Step'])
    df['hours_elapsed'] = (df['Wall time'] - df['Wall time'][0])/3600
    df = df.drop(columns=['Wall time'])
    df = df.rename(columns={'Value':'validation_accuracy'})
    df.to_csv(fil, index_label="epoch")
  return

def fix_errors():
  files = glob.glob("./*/*.csv")
  for fil in files:
    df = pd.read_csv(fil)
    df = df.rename(columns={'Value':'validation_accuracy'})
    df.to_csv(fil,index=False)

def union_subsets():
  files = glob.glob("./smaller/*.csv")
  dfs = []
  for fil in files:
    dfs.append(pd.read_csv(fil))
  union = pd.concat(dfs)
  union.to_csv("./smaller/smaller_union.csv",index=False)

def subset_observations():
  df = pd.read_csv("./smaller/smaller_union.csv")
  df = df.loc[df["epoch"] % 10 == 0]
  df.to_csv("./smaller/smaller_subset.csv", index=False)

def median_values():
  files = glob.glob("./*/*_union.csv")
  for fil in files:
    df = pd.read_csv(fil)
    df = df.groupby(['epoch']).median()
    df.to_csv(fil[0:-9] + "medians.csv")

def min_values():
  files = glob.glob("./*/*_union.csv")
  for fil in files:
    df = pd.read_csv(fil)
    df = df.groupby(['epoch']).min()
    df.to_csv(fil[0:-9] + "mins.csv")

def max_values():
  files = glob.glob("./*/*_union.csv")
  for fil in files:
    df = pd.read_csv(fil)
    df = df.groupby(['epoch']).max()
    df.to_csv(fil[0:-9] + "maxs.csv")

def mean_values():
  files = glob.glob("./*/*_union.csv")
  for fil in files:
    df = pd.read_csv(fil)
    df = df.groupby(['epoch']).max()
    df.to_csv(fil[0:-9] + "means.csv")

def differences_in_accuracies(): 
  smaller_means = pd.read_csv("./smaller/smaller_means.csv")
  affine_means = pd.read_csv("./affine/affine_means.csv")
  df = affine_means.set_index("epoch").subtract(smaller_means.set_index("epoch")).drop(columns=["hours_elapsed"])
  df.to_csv("./affine_minus_smaller_accuracies.csv")

def main():
  differences_in_accuracies()

if __name__ == "__main__":
  main()
