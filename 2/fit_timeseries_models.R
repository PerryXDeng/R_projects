source("./2/utlity.R")
source("./2/holt.R")

df = load_combined_areas_data()

mat = transposed_time_series_matrix(df, "POPESTIMATE")

selected_vector = unlist(mat[1532])
Holt(selected_vector)
Holt(selected_vector, type="multiplicative")
names = get_location_names(df)
print(names[1532])
