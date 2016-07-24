find . -name "ZOHCRM008.CSV" -print
find . -name "*odbc.ini*" -print
find . -name "*CRM008*" -print
find . -name "DS_UPD_IP_CDCRM2.ds" -print
find . -name "uvodbc.config" -print
find . -name "CL_VAL_COMMON.ds" -print
ls -al | grep FecProcLeasing.ds
cd /interfaces/hub/desa_hub_nal02/work/lego
touch -c FecProcLeasing.ds
sftp -b  #$PATH_INPUT#/batchfilecrm4 #$V_CON_USER_BW#@#$V_CON_SERVER_BW#:#$V_CON_PATH_BW# >> #$PATH_INPUT#/listadoCrm4.txt
grep -Ril /opk/IBM/InformationServer/Server/ "DWH"
grep -Ril "LEASING_SBD" .
istat .odbc.ini
chown dsadm:dstage /opk/IBM/InformationServer91/Server/DSEngine/.odbc.ini
perl -pi -e 's/~//g' *
perl -pi -e 's/\|/~/g' *



cd /interfaces/hub/desa_hub_nal02/work/lego
sh JOB_HUB_EXT_OPE_CORE_IP.sh
sh JOB_HUB_CDC_OPE_CORE_IP.sh
sh JOB_HUB_CAR_OPE_CORE_IP.sh
sh JOB_HUB_CAR_OPE_CORE_IP_UPD.sh
sh JOB_EXT_IP.sh
sh JOB_TRF_IP.sh
sh SEQ_COMMON.sh
sh JOB_EXT_COMMON_LO.sh
sh JOB_CLE_IP.sh
sh JOB_LOD_INSERT_IP.sh
sh JOB_EXT_IP.sh && sh JOB_TRF_IP.sh && sh SEQ_COMMON.sh && sh JOB_EXT_COMMON_LO.sh && sh JOB_CLE_IP.sh && sh JOB_LOD_INSERT_IP.sh
perl -pe 's|\n|\r\n|' old.txt > new.txt
PS1="`ps -o user= -p $$ | awk '{print $1}'`-$PWD$"
od -xc a.txt #Show file
wc -lc filename #Line count
iostat # I/O
ps -ef | grep ds
who | cut -d' ' -f1
ls -l | grep -i "^.\{45\}Mar 22" | cut -c59-
[ -z "$PWD" ]
perf stat -d -p PID sleep 30

echo $(( `wc -l abc | awk '{print $1}'`-`cat abc | grep -n EnvVarValues | cut -d':' -f1` ))

tail -n $(( `wc -l abc | awk '{print $1}'`-`cat abc | grep -n EnvVarValues | cut -d':' -f1` )) abc >> def
perl -pe 's|\r\n|\n|' test.txt > t2.txt
perl -pe 's/"//g' input.csv > output.csv
head -n1 a.txt
find . -name "DEL_DS_DELTAS_BANCO_LO_LO.TXT" -print
sftp -b  file user@server:path > file2


sed -n -e 's/^.*\(&"$STAGING_OWN_DMR"\)/\1/p' TOT.dsx | cut -f2- -d'.' | awk '{print $1}' | sed "s/'/ /g" | sort | uniq > axef.txt

sed '8q;d' del_ds.sh #Extrae linea 8 del archivo
date +"%d%m%Y-%H:%M:%S"
date +"%y%m%d"
npm config set proxy http://172.25.25.9:8080