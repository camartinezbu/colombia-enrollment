# Proposed dataset for the Beggining Data Storytelling Case Study course on Datacamp
library(tidyverse)

# Source: https://www.datos.gov.co/Educaci-n/MEN_MATRICULA_ESTADISTICA_ES/5wck-szir
# Dataset containing the enrollment for every higher education program in Colombia
# registered at the Ministry of Education, disaggregated by year, semester, 
# institution, program, type of program, and sex of the people enrolled
enrollment <- read_csv("data/raw/MEN_MATRICULA_ESTADISTICA_ES.csv") |> 
  transmute(
    Year = Año,
    Semester = Semestre,
    Id_Institution = `Código de la Institución`,
    Name_Institution = `Institución de Educación Superior (IES)`,
    Id_Area,
    Id_Program = `Código SNIES delprograma`,
    Name_Program = `Programa Académico`,
    Id_Level = `Id_Nivel_Formacion`,
    Sex = `Id Género`,
    Enrolled = `Total Matriculados`)

# Source: https://www.datos.gov.co/Educaci-n/MEN_PROGRAMAS_DE_EDUCACI-N_SUPERIOR/upr9-nkiz
programs <- read_csv("data/raw/MEN_PROGRAMAS_DE_EDUCACI_N_SUPERIOR.csv") 

knowledge_areas <- programs |> 
  select(Id_Area = codigoareaconocimiento, Name_Area = nombreareaconocimiento) |> 
  distinct(Id_Area, Name_Area) |> 
  filter(!is.na(Name_Area)) |> 
  mutate(Name_Area = case_when(
    Name_Area == "Economía administración contaduría y afines" ~ "Economics, Administration, Accounting and related",
    Name_Area == "Ciencias sociales y humanas" ~ "Social and human sciences",
    Name_Area == "Ingeniería arquitectura urbanismo y afines" ~ "Engineering, architecture, urbanism and related",
    Name_Area == "Bellas artes" ~ "Arts",
    Name_Area == "Matemáticas y ciencias naturales" ~"Math and natural sciences",
    Name_Area == "Agronomía veterinaria y afines" ~"Agronomy, veterinary medicine and related",
    Name_Area == "Ciencias de la educación" ~ "Education sciences",
    Name_Area == "Ciencias de la salud" ~ "Health sciences",
    Name_Area == "Sin clasificar" ~ "Not classified"
  ))

program_levels <- programs |> 
  select(Id_Level = codigonivelformacion, Name_Level = nombrenivelformacion) |> 
  distinct(Id_Level, Name_Level) |> 
  filter(!is.na(Name_Level)) |> 
  mutate(Name_Level = case_when(
    Name_Level == "Universitaria" ~ "Undegraduate",
    Name_Level == "Doctorado" ~ "PhD",
    Name_Level == "Maestría" ~ "Master",
    Name_Level == "Tecnológica" | Name_Level == "Formación técnica profesional" ~ "Technical or Technological",
    TRUE ~ "Specialization"
  )) |> 
  distinct(Id_Level, Name_Level) 
  
# Final dataset
final_dataset <- enrollment |> 
  left_join(knowledge_areas, by = "Id_Area") |> 
  left_join(program_levels, by = "Id_Level") |> 
  select(Year, Semester, Id_Institution, Name_Institution, Id_Level, Name_Level,
         Id_Area, Name_Area, Id_Program, Name_Program, Sex, Enrolled)

write_csv(final_dataset, "data/final/enrollmnent-colombia.csv")
  