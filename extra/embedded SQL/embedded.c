#include<stdio.h> 
#include<string.h> 
EXEC SQL BEGIN DECLARE SECTION; 
long station_id; 
long mon; 
float temp; 
float rain; 
char city_name[21]; 
long SQLCODE;
EXEC SQL END DECLARE SECTION; 
main() 
{ 
/* the CONNECT statement, if needed, goes here */ 
strcpy(city_name,"Denver"); 
EXEC SQL SELECT ID INTO :station_id 
FROM STATION 
WHERE CITY = :city_name;
if (SQLCODE == 100) 
{ 
printf("There is no station for city %s\n",city_name); 
exit(0); 
}
printf("For the city %s, Station ID is %ld\n",city_name,station_id);  
printf("And here is the weather data:\n"); 
EXEC SQL DECLARE XYZ CURSOR FOR 
SELECT MONTH, TEMP_F, RAIN_I 
FROM STATS 
WHERE ID = :station_id 
ORDER BY MONTH;
EXEC SQL OPEN XYZ; 
while (SQLCODE != 100) { 
EXEC SQL FETCH XYZ INTO :mon, :temp, :rain; 
if (SQLCODE == 100) 
printf("end of list\n");
else 
printf("month = %ld, temperature = %f, rainfall = %f\n",mon,temp,rain);
}
EXEC SQL CLOSE XYZ; 
exit(0); 
}
