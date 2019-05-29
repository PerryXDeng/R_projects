import csv

INPUT_FILENAME = r"path\to\csci620_big_data_homework\1\datasets\AppleStore.csv"
OUTPUT_FILENAME = r"AppleStore_filtered.csv"

COLUMN = "rating_count_ver"
MIN_VALUE = 30

with open(INPUT_FILENAME, "r", encoding="utf8") as input_file:
    with open(OUTPUT_FILENAME, "w", encoding="utf8", newline="") as output_file:

        # Setup reader and writer
        input_reader = csv.DictReader(input_file)
        output_writer = csv.DictWriter(output_file, input_reader.fieldnames)
        output_writer.writeheader()

        # Filter rows
        for row in input_reader:
            if int(row[COLUMN]) >= MIN_VALUE:
                output_writer.writerow(row)

