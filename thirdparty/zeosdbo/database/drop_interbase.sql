/*==============================================================*/
/* Database name:  zeoslib.gdb                                  */
/* DBMS name:      Interbase 6                                  */
/*==============================================================*/
drop procedure ABTEST;
drop procedure procedure2;
drop procedure procedure1;
drop procedure procedure_upd_people_A;
drop procedure procedure_upd_people_B;
drop procedure procedure_upd_people_C;

drop view dep_view;
drop table blob_values;
drop table cargo;
drop table people;
drop table equipment2;
drop table equipment;
drop table date_values;
drop table department;
drop table number_values;
drop table string_values;
drop table not_null_values;
drop table "Case_Sensitive";
drop table "Spaced Names";
drop table case_sensitive;
drop table high_load;
drop table default_values;
drop table default_values2;
drop table domain_values;

drop domain tinteger;
drop domain tfloat;
drop domain tstring;
drop generator "GEN_ID";
