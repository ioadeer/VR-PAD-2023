
# Ejemplo de Chat GPT

# filename="S1234_example_filename.txt"  # replace with your actual file name
# 
# IFS="_" read -ra parts <<< "$filename"  # split filename into parts using "_" as separator
# 
# # extract the number after "S" from the first part
# number=${parts[0]#S}
# 
# if [[ "$number" -eq 1234 ]]; then
#   echo "The number after S is equal to 1234"
# elif [[ "$number" -gt 1234 ]]; then
#   echo "The number after S is greater than 1234"
# else
#   echo "The number after S is less than 1234"
# fi
# todos archivos en directorio
for f in *.csv; do
  IFS="_" read -ra parts <<< "$f" # parseamos numbre
  number=${parts[0]#S} # parseamos id
  # operacion logica
  # hasta sujeto 11 habiamos hecho 3 bloques
  if [[ $number -le 11 ]]; then 
    echo "Sujetos de 1 a 11 moviendo a carpeta"
    mv $f 1_11_subject
  elif [[ "$number" -ge 22  &&  "$number" -le 26 ]]; then
    echo "Sujetos de 22 a 26 moviendo a carpeta"
    mv $f 22_26_subject
  elif [[ "$number" -ge  27  &&  "$number" -le  34 ]]; then
    echo "Sujetos de 22 a 26 moviendo a carpeta"
    mv $f 27_34_subject
  else
    echo "File does not enter in categorization"
  fi
done

