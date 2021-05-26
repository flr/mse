mod0.17.dat
Model_1.0_2stk # As model 1.5 from SC06, updated for SC07
#	Number	of	stocks
2
#	Names	of	stocks
Stock1%Stock2
# Selectivity sharing vector (number_fisheries + number_surveys)  
#Fsh_1 Fsh_2 Fsh_3 Fsh_4 Srv_1 Srv_2 Srv_3 Srv_4 Srv_5 Srv_6 Srv_7 Srv_8  
#N_Chile_Fsh CS_Chile_Fsh FarNorth_Fsh Offshore_Trawl_Fsh Chile_AcousCS Chile_AcousN Chilean_CPUE DEPM Peru_Acoustic Peru_CPUE Chinese_CPUE Offshore_CPUE #  
# 2 3 4 1 2 3 4 5 6 7 8
1 1 2 1 1 1 1 1 2 2 1 1
1 1 1 1 2 2 1 2 1 1 1 1
1 2 3 4 1 2 2 4 3 3 4 4
#Number of regimes (by stock)
1
2
#Sr_type 
2  
#AgeError 
0  
#Retro 
0  
#Recruitment sharing matrix (number_stocks, number_regimes)
1
2 3
#Steepness 
0.65 0.65 0.65
300 300 300
-6 -6 -6
#SigmaR 
0.6 0.6 0.6
15 15 15
-4 -4 -4
#phase_Rzero
4 4 4
#Nyrs_sr
17
27
16
#yrs_sr
	2000	2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	2011	2012	2013	2014	2015	2016
1970	1971	1972	1973	1974	1975	1976	1977	1978	1979	1980	1981	1982	1983	1984	1985	1986	1987	1988	1989	1990	1991	1992	1993	1994	1995	1996
2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	2011	2012	2013	2014	2015	2016
#reg_shifts blank if nreg==1

1999
#Growth parameters sharing matrix (number_stocks, number_regimes)
1
2 2
#Linf
74.4 80.4
0.1 0.1	
-4 -4																																									
#K
0.16 0.16	
0.1 0.1	
-4 -4																																									
#Lo_Len
18 18	
0.1 0.1	
-4 -4																																									
#Sigma_len
0.09 0.09 
0.1 0.1	
-4 -4																																									
#Mortality sharing matrix (number_stocks, number_regimes)
1
2 2
#Natural_Mortality 
0.23 0.33
0.05 0.05
-4 -4
# NEW npars_mage
0 0
# NEW ages_M_changes

# NEW Mage_in

# phase_Mage
-5 -5
#Phase_Random_walk_M 
-4
-4
#Nyrs_Random_walk_M 
0
0
#Random_walk_M_yrs blank if nyrs==0

#Random_walk_M_sigmas blank if nyrs==0

#catchability 
1.143881847	0.029045376	0.000159487	0.519171913	0.002284038	0.007987171	0.00033275	0.033884372
12  12  12  12  12  12  12  12 
3  5  3  3  3  4  4  4
#q_power                    
1  1  1  1  1  1  1  1 
1.2  1.2  1.2  1.2  1.2  1.2  1.2  1.2
-1  -1  -1  -1  -1  -1  -1  -1
#Random_walk_q_phases                    
1  -1  1  -1  1  -1  -1  -1
#Nyrs_Random_walk_q
1  0  1  0  0  0  0  0
#Random_walk_q_yrs blank if nyrs==0
2002 
2000
#Random_walk_q_sigmas blank if nyrs==0
2.0
2.0
#q_agemin                    
2  2  2  2  1  1  2  2
#q_agemax                    
10  10  10  10  6  6  10  10
#use vb wt age                    
0
#n_proj_yrs                    
10  
#---------------------------------------------------------
# Fishery 1 N Chile  
1  #selectivity type
9  #n_sel_ages
2  #phase sel
1  #curvature penalty
1  #Dome-shape penalty
# Years of selectivity change Fishery 1 N Chile  
35																														
1984	1985	1986	1987	1988	1989	1990	1991	1992	1993	1994	1995	1996	1997	1998	1999	2000	2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	2011	2012	2013    2014    2015    2016	2017	2018
0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5 0.5 0.5 0.5 0.5 0.5
# Initial values for coefficitients at each change (one for every change plus 1)  
# 2 3 4 5 6 7 8 9 10 11 12  
0.2 0.7 1 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Fishery 2, Central South Chile  
1																														
10																														
3																														
1																														
25																														
#	Years	of	selectivity	change	Fishery	2,	Central	South	Chile																					
36																														
1984	1985	1986	1987	1988	1989	1990	1991	1992	1993	1994	1995	1996	1997	1998	1999	2000	2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	2011	2012	2013	2014	2015	2016	2017	2018	2019
0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5	0.5 0.5 0.5 0.5 0.5 0.5 0.5
#	Initial	values	for	coefficitients	at	each	change	(one	for	every	change	plus	1)																	
#	2	3	4	5	6	7	8	9	10	11	12																			
0.2	0.7	1	1	1	1	1	1	1	1	1	1																			
#---------------------------------------------------------
# Fishery 3 Peru  
1  
7  
4  
1  
12.5  
# Years of selectivity change Fishery 3 Peru  
1
2002
0.5
# Initial values for coefficitients at each change (one for every change plus 1)  
# 2 3 4 5 6 7 8 9 10 11 12  
0.2 0.7 1 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Fishery 4 International  
1  
9  
3  
1  
12.5  
# Years of selectivity change Fishery 4 International  
30
1980	1981	1982	1983	1984	1985	1986	1987	1988	1989	1990	1991	2000	2001	2002	2003	2006	2007	2008	2009	2010	2011	2012	2013	2014	2015	2016	2017	2018	2019
0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 .5 .5 .5 .5 .5 .5 .5
# Initial values for coefficitients at each change (one for every change plus 1)  
# 2 3 4 5 6 7 8 9 10 11 12  
0.2 0.7 1 1 1 1 1 1 1 1 1 1
#---------------------------------------------------------  
# Index number 1 AcousCS  
1  
10  
2  
0.25  
100  
1
2005
0.7
0.3 1 1 1 1 1 1 1 1 1 1 1
#---------------------------------------------------------  
# Index number 2 Acous_N  
1  
10  
5  
0.25  
100  
# Years of selectivity change 
2 
2012 2016
0.5 0.5
0.3 1 1 1 1 1 1 1 1 1 1 1 
#--------------------------------------------------------- 
# Index number 3 Chile_CPUE  
1  
10  
-5  
0.25  
100  
0 
0.8 1 1 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Index number 4 DEPM  
1  
10  
3  
0.25  
100 # 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010
1
2003
0.7
0.04 0.43 0.93 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Index number 5 Acoustic_Peru  
1  
10  
-5  
0.25  
100  
0
0.04 0.43 0.93 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Index number 6 Peru_CPUE  
1  
10  
-5  
0.25  
100  
0 
0.04 0.43 0.93 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Index number 7 EU_CPUE  
1  
10  
-5  
0.25  
100  
0 
0.04 0.43 0.93 1 1 1 1 1 1 1 1 1  
#---------------------------------------------------------
# Index number 8 EU_CPUE  
1  
10  
-5  
0.25  
100  
0
0.04 0.43 0.93 1 1 1 1 1 1 1 1 1
#---------------------------------------------------------
#Population	Weight	at	Age	1000																																							
0.051	0.0884	0.134	0.195	0.2612	0.328	0.4184	0.5344	0.6814	0.8426	1.0705	1.4589
0.038	0.146	0.324	0.555	0.819	1.100	1.384	1.660	1.922	2.165	2.387	2.588
#Maturity	at	Age	6	7	8	9	10	11	12																																		
0.072426485	0.312168669	0.725119498	0.938773837	0.988903862	0.998073265	0.999667986	0.999942863	0.999990169	0.999998309	0.999999709	0.99999995
0	0.37	0.98	1	1	1	1	1	1	1	1	1
#Test  
123456789  

