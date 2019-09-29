import pandas as pd
import glob

def index_epoch_and_offset_time():
  files = glob.glob("./*/*.csv")
  for fil in files:
    df = pd.read_csv(fil, index_col=False, header=0)
    df = df[0:101]
    df = df.drop(columns=['Step'])
    df['hours elapsed'] = (df['Wall time'] - df['Wall time'][0])/3600
    df = df.drop(columns=['Wall time'])
    df = df.rename(columns={'Value':'validation accuracy'})
    df.to_csv(fil, index_label="epoch")
  return

def fix_errors():
  files = glob.glob("./*/*.csv")
  for fil in files:
    df = pd.read_csv(fil)
    df = df.rename(columns={'Value':'validation accuracy'})
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

def main():
  subset_observations()

if __name__ == "__main__":
  main()
