# Getting percentages and idea of which nodes are in the game groups across different channels
    #  Getting groups of nodes based on categories defined in "Story_PostRound1Review.ppt"
    # 1. Active users - nodes with larger out degree
    # 2. Middle of the road - nodes with larger in degree, but no negative out degree
    # 3. Passive users - nodes with larger in degree, no out degree

######## GROUP 1 
G1_advmech <- c("U1YCYCK61", "U4N910YF8", "UBMTZF0UA", "UJD3KK9NX")
G1_advtherm <- c("U1YEMA4BA", "U1YKN36HF")
G1_classped <- c("U1YJ38A8K", "U1YKC618E" ,"U1YKM7ZRT" ,"U721GN133" ,"U815X75JT", "UBMBY393N" ,"UFJ13UD3N","UJNH8230Q")
G1_jupyter <- c("U1YCYCK61", "U1YEMA4BA" ,"U1YKC618E", "U1YKKMTFE", "U1YKM7ZRT", "U1YKMC1QD" ,"U243ZCBUZ" ,"U4LPDPNM9", "U4PL0CRNG", "U4WAE03GW", "U57AJGYMD", "U5B3592R1", "U5YT9G1LH", "U6X4Q5919" ,"U721GN133", "U9EAC5EF3", "UBN4DAZ9P", "UF0SNUWLE", "UJNH8230Q")
G1_glowscript <- c("U014VFQKHMW", "U1YCYCK61", "U1YKEP2PQ" ,"U1YKKMTFE", "U1YKMC1QD", "U243ZCBUZ","U46RH1GE4", "U4X9ZLJ2D", "U5B5AU4TS", "U6X4Q5919", "U9N0B9JFR" ,"UBMBY393N" ,"UF0SNUWLE", "UJNH8230Q" ,"UN7RYC5T4")
G1_trinket <- c("U1YKH1T96" ,"U1YKM7ZRT" ,"U1YKMC1QD" ,"U4X9ZLJ2D" ,"UBN8AAZS7" ,"UF0SNUWLE", "UFJ13UD3N" ,"UJF6E99FF", "UM196HCDB")
G1_wwdt <- c("U1YKEP2PQ" ,"U1YKMC1QD" ,"U4LPDPNM9", "U9N0B9JFR")
G1_umw <- c("U1YCYCK61" ,"U1YKN36HF", "U1YKUG3B7", "U1YKVQNBF", "U4L8L347M", "U4M7UH1QW", "U4WAE03GW" ,"U57AJGYMD")

GP1_list <- list(G1_advmech, G1_advtherm, G1_classped, G1_jupyter, G1_glowscript, G1_trinket, G1_wwdt, G1_umw)
names(GP1_list) <- c("G1_advmech", "G1_advtherm", "G1_classped", "G1_jupyter", "G1_glowscript",  "G1_trinket", "G1_wwdt", "G1_umw")


######## GROUP 2
G2_advmech <- c("U1YJ38A8K")
G2_advtherm <- c("U1YCYCK61" ,"U1YKKMTFE", "U1YKWAY31" ,"U243ZCBUZ" ,"U4AG8DTQA")
G2_classped <- c("U1YCYCK61", "U1YEMA4BA" ,"U46RH1GE4" ,"U5BA8P4NP", "UBP0NQWLD", "UPP8DBQ6N")
G2_jupyter <- c("U1YJ38A8K", "U1YKEP2PQ" ,"U1YKH1T96", "U1YKN36HF", "U1YKUG3B7" ,"U1YKVQNBF", "U1YKZNK71", "U1YL4RMRQ", "U2SSRTE8K", "U4D32QHFT","U57M23EJD" , "U5B5AU4TS", "U5BA8P4NP", "U5BM2DRQT" ,"UBMMLALTE" ,"UBMTZF0UA" ,"UFJ13UD3N" ,"UKQ7M9D6W")
G2_glowscript <- c("U1YEMA4BA" ,"U1YKC618E", "U1YKH1T96" ,"U1YKM7ZRT" ,"U1YKN36HF" ,"U1YKUG3B7", "U1YKW1FKR" ,"U1YKWBN2W", "U1ZD61Q06", "U47FDDCBV" ,"U4GGL6MK3", "U4GVBA31T", "U815X75JT" ,"UBLTXH34G", "UBMFPP7MZ" ,"UDG9865MJ","UFJ13UD3N", "UKQ7M9D6W", "UKRH1R7KL" ,"UKTKMDG2E" ,"UM196HCDB")
G2_trinket <- c("U1YKC618E" , "U4GGL6MK3" ,"U5SCU625N", "U6X4Q5919", "UBMTZF0UA")
G2_wwdt <- c("U1YCYCK61", "U1YKC618E", "UBMMLALTE" ,"UBN8AAZS7", "UBNVC491V" ,"UBQ5PDK8Q" )
G2_umw <- c("U1YKC618E" ,"U3MHE3VLY", "U3X4493KL", "U4M2X0HRD" ,"U4WDXT12S" ,"U4XCWG93N", "U4Z31MLE9" ,"U4ZTN8UDT" ,"U511VJ6BV" ,"U51HYHY56" ,"U55RMEJ8G")

GP2_list <- list(G2_advmech, G2_advtherm, G2_classped, G2_jupyter, G2_glowscript, G2_trinket, G2_wwdt, G2_umw)
names(GP2_list) <- c("G2_advmech", "G2_advtherm", "G2_classped", "G2_jupyter", "G2_glowscript",  "G2_trinket", "G2_wwdt", "G2_umw")



######## GROUP 3 
G3_advmech <- c("U1YKC618E" ,"U1YKH1T96", "U1YKN5ZV5", "U1YL4RMRQ", "U3XKPSH5Z" ,"U4862UVGW", "U55RMEJ8G" ,"U5ANCPHPB" ,"U5B3773AP" ,"U5B5GA874" ,"U5BA8P4NP", "U6CFMB7PC", "U6D7T2UPJ" ,"U6X4Q5919" ,"U76LDPXC7", "UBN8AAZS7" ,"UBNRR0FML" ,"UBP0NQWLD", "UBQBQDPEV")
G3_advtherm <- c( "U1YJ38A8K", "U1YKW1FKR" ,"U1YL4RMRQ", "U47FDDCBV","U57AJGYMD" ,"U5B3773AP”", "U5BA8P4NP", "U6CFMB7PC", "U76LDPXC7")
G3_classped <- c("U1YKN36HF", "U1YKN5ZV5" ,"U1YKNQH55", "U1YKW1FKR", "U1YL4RMRQ", "U1ZD61Q06","U2UEEVDNZ" ,"U2Y0DKVD2", "U3XKPSH5Z" ,"U47FDDCBV" ,"U4HFYQYKY", "U4HHS0F71", "U4LEWCMBK" ,"U4WAE03GW" ,"U4X9ZLJ2D", "U57AJGYMD" ,"U5B3773AP" ,"U5B5GA874", "U5BM2DRQT","U1YKKMTFE" ,"U4D32QHFT","U57M23EJD", "U76LDPXC7", "U9EAC5EF3","U9G9Z7M71", "UAT9EUJLC" ,"UBMFPP7MZ" ,"UBMMLALTE", "UBMP43DQV", "UBMPAERMJ", "UBMTZF0UA" ,"UBQBQDPEV" ,"UBQC15LS1", "UBQQ61UCB" ,"UC0TC6482","UJYL5SNH3", "UKQ7M9D6W" ,"UKRH1R7KL" ,"UKTJ7SUGJ","UKTKMDG2E", "UKV3ZR9J5" ,"UKW9LLVND", "UL9HNHJ0M", "ULAEURNV6")
G3_jupyter <- c("U1YJ1ALLT", "U1YJA4JNP" ,"U1YKKBNCU", "U1YKN5ZV5", "U1YKWAY31" ,"U1YKWBN2W",  "U1YKWRURG", "U1YKX46BH", "U1YL11QBH" ,"U1YLBPNM7" ,"U1ZD61Q06", "U3XKPSH5Z", "U47FDDCBV", "U4GGP2601", "U4GV6AJ3D", "U4HFYQYKY" ,"U4K1D1E91", "U4L8L347M", "U4XCWG93N", "U5B3773AP", "U5B5GA874" ,"U5B5RJKK2" ,"U5Z24K7P0" ,"U6CFMB7PC", "U6D7T2UPJ", "U76LDPXC7", "U4GGL6MK3", "U4M7UH1QW", "U4X9ZLJ2D","U635YQ6K1", "U815X75JT", "U9G9Z7M71", "UAT9EUJLC", "UBMBY393N" ,"UBMP43DQV" ,"UBN8AAZS7", "UBNRR0FML", "UBQ05DLJ2" ,"UBQC15LS1" ,"UBZUWCG5D", "UC0TC6482" ,"UD3S5UR6E", "UH3JDKVQA", "UJ24NAAN5" ,"UJYL5SNH3", "UKQKQCN4U", "UKRH1R7KL", "UKS3G4A1K", "UKV3ZR9J5", "UL2KVMYBF", "UL61Y3FHN" ,"UL7B54G6T", "UL9HNHJ0M" ,"ULEGD2YD7", "UN7RYC5T4" ,"UN9GWT6MD" ,"U010DPQMG3T", "U012P64UW9K", "U012R0SN87N", "U5ZA1BG5B", "U7NM3PWJF", "UBMPAERMJ", "UDG9865MJ", "UM196HCDB", "USUCLDU6L", "U011XE04C6M" ,"U014Q655M1Q", "U014VFQKHMW", "U010HJ8QCDB", "U0164GWNVU2", "U0166P9U1M1" ,"U016AUFHRV5" ,"U016G4CRUK1" ,"U016RT0SENP" ,"U016USX6YRW", "U0171CN8W4V" ,"U01749V05UZ" ,"U01757V875E", "U0177QF6BEU", "U018S27CF2N", "U018T91P469", "UL9HK536H")
G3_glowscript <- c("U010DPQMG3T", "U011XE04C6M" ,"U012R0SN87N", "U016AUFHRV5" ,"U016G4CRUK1", "U0171CN8W4V", "U01749V05UZ" ,"U01757V875E", "U0177QF6BEU" ,"U018S27CF2N", "U018T91P469", "U1YJA4JNP" ,"U1YKKBNCU", "U1YKN5ZV5", "U1YKNQH55", "U1YKWRURG", "U1YKZNK71", "U1YL4RMRQ" ,"U1YLBPNM7", "U2SSRTE8K", "U3X4493KL" ,"U3XKPSH5Z","U3YBRL9P1", "U4HFYQYKY" ,"U4HHS0F71" ,"U4PL0CRNG", "U4XCWG93N","U5B5GA874", "U5BM2DRQT", "U5ZA1BG5B" ,"U6CFMB7PC" ,"U7NM3PWJF", "UBC3EG8KG" ,"UBMPAERMJ", "UBMTZF0UA" ,"UBN8AAZS7", "UBQQ61UCB" ,"UC0TC6482", "UD3S5UR6E", "UJYL5SNH3", "UKQ1E11QB" ,"UKV3ZR9J5", "UL20S8CAH", "UL2KVMYBF", "UL61Y3FHN" ,"UL9F42XCK", "UL9HNHJ0M" ,"UN9GWT6MD" ,"UPP8DBQ6N" ,"USUCLDU6L", "U010HJ8QCDB", "U0166P9U1M1","U017W6G7US0", "U01GJ5KFDST" ,"UBRN81YJC")

G3_trinket <- c("U1YEMA4BA", "U1YKN5ZV5" ,"U4XCWG93N", "U5B5AU4TS", "U5B5GA874" ,"U6D7T2UPJ", "UBMPAERMJ", "UBQQ61UCB" ,"UC0TC6482" ,"UJNH8230Q", "UJYL5SNH3" ,"UKRH1R7KL", "UL20S8CAH" ,"U815X75JT", "UBMBY393N","U010HJ8QCDB", "U0166P9U1M1", "U016G4CRUK1" ,"U01APQC8LLQ", "U2SSRTE8K", "U5ZA1BG5B", "UBC3EG8KG", "U017W6G7US0")
G3_wwdt <- c("U1YKN5ZV5", "U4X9ZLJ2D" ,"UBMPAERMJ" ,"U1YKUG3B7", "U57M23EJD", "U5B5RJKK2", "UAT9EUJLC", "UBMFPP7MZ" ,"UBMTZF0UA", "UBN4DAZ9P", "UBQ05DLJ2" ,"UBQC15LS1" ,"UBQQ61UCB")
G3_umw <- c("U4NKMDWF9","U4QTL4RPC","U4ZKT4TMZ", "U502466QK", "U52CJAR96", "U55PZCVD5", "U57A63VKQ" ,"U57ALEQH5", "U1YKEP2PQ" ,"U1YKH1T96" ,"U1YKN5ZV5")

GP3_list <- list(G3_advmech, G3_advtherm, G3_classped, G3_jupyter, G3_glowscript, G3_trinket, G3_wwdt, G3_umw)
names(GP3_list) <- c("G3_advmech", "G3_advtherm", "G3_classped", "G3_jupyter", "G3_glowscript",  "G3_trinket", "G3_wwdt", "G3_umw")

############## CHANNEL LIST
advmech <-  c(G1_advmech, G2_advmech, G3_advmech)
advtherm <-  c(G1_advtherm, G2_advtherm, G3_advtherm)
classped <-  c(G1_classped, G2_classped, G3_classped)
jupyter <-  c(G1_jupyter, G2_jupyter, G3_jupyter)
glowscript <-  c(G1_glowscript, G2_glowscript, G3_glowscript)
trinket <-  c(G1_trinket, G2_trinket, G3_trinket)
wwdt <-  c( G1_wwdt, G2_wwdt, G3_wwdt)
umw <-  c(G1_umw, G2_umw, G3_umw)

channel_list <- list(advmech, advtherm, classped, jupyter, glowscript, trinket, wwdt, umw)

names(channel_list) <- c("advmech", "advtherm", "classped", "jupyter", "glowscript",  "trinket", "wwdt", "umw")



# Get all nodes from sublist in one list
get_completenodelist <- function(group_list, complete_nodelist){
    for (name in names(group_list)){
        for (i in group_list[[name]]){
            if (!(i %in% complete_nodelist)){
                complete_nodelist <- c(complete_nodelist, i)
            }
        }
    }
    return(complete_nodelist)
}


# For each node in combined list
    # Are they in 75% of gp 1 lists
    # Are they in 75% of gp 2 lists
    # Are they in 75% of gp 3 lists
check_otherchannels_samegroup <- function(node_matrix, GP_list, gp_colname){
    total_channels <- length(GP_list)

    num_channels <- length(names(GP_list))
    for (n in 1:nrow(node_matrix)){
        node_count <- 0
        node_name <- node_matrix[n,"nodes"]
        # Iterate over all lists in GP list and see if this node is there
        for (channelname in names(GP_list)){
            if (node_name %in% GP_list[[channelname]]){
             # Count how many times the above happens 
                node_count <- node_count + 1
            }
        }
        # Get percentage
        perc <- node_count/num_channels
        # Update matrix value accordingly 
        node_matrix[n, gp_colname] <- perc
    }

    return (node_matrix)
}

# Check if a user is in multiple channels - report counts and percentages
check_multiplechannels <- function(node_matrix, channel_list){
    total_channels <- length(channel_list)
    print(total_channels)

    for (n in 1:nrow(node_matrix)){
        node_count <- 0
        node_name <- node_matrix[n,"nodes"]
        # Iterate over all lists in GP list and see if this node is there
        for (channelname in names(channel_list)){
            if (node_name %in% channel_list[[channelname]]){
             # Count how many times the above happens 
                node_count <- node_count + 1
            }
        }
        # Get percentage
        perc <- node_count/total_channels
        # Update matrix value accordingly 
        node_matrix[n, "count"] <- node_count
        node_matrix[n, "perc"] <- perc
    }

    return (node_matrix)
}


# Some cross group checking, neets thresholds and such 



main <- function(GP1_list, GP2_list, GP3_list){
    # Get complete node list
    complete_nodelist_init <- c()
    complete_nodelist_gp1 <- get_completenodelist(GP1_list, complete_nodelist_init)
    complete_nodelist_gp2 <- get_completenodelist(GP2_list, complete_nodelist_gp1)
    complete_nodelist <- get_completenodelist(GP3_list, complete_nodelist_gp2)

    print(complete_nodelist[0:5])

    # Create df where users are rows, and entries are perc values

    grps_perc <- data.frame(nodes=complete_nodelist,
                             gp1_inperc=rep(NA, length(complete_nodelist)),
                             gp2_inperc=rep(NA, length(complete_nodelist)),
                             gp3_inperc=rep(NA, length(complete_nodelist))
    )

    grps_perc_post1 <- check_otherchannels_samegroup(grps_perc, GP1_list, "gp1_inperc")
    grps_perc_post2 <- check_otherchannels_samegroup(grps_perc_post1, GP2_list, "gp2_inperc")
    grps_perc_final <- check_otherchannels_samegroup(grps_perc_post2, GP3_list, "gp3_inperc")
    print(grps_perc_final[0:5,])

    # Use matrix slicing to get % of which values are true
    grp1_morethan_50perc <- grps_perc_final[grps_perc_final["gp1_inperc"] >= 0.50, ] 
    grp2_morethan_50perc <- grps_perc_final[grps_perc_final["gp2_inperc"] >= 0.50, ]
    grp3_morethan_50perc <- grps_perc_final[grps_perc_final["gp3_inperc"] >= 0.50, ]
    # print(grp1_morethan_50perc)
    # print(grp2_morethan_50perc) 
    # print(grp3_morethan_50perc)

    print(paste("Number of nodes in gp1 50% or more of the time across the 8 channels:", nrow(grp1_morethan_50perc)))
    print(paste("Number of nodes in gp2 50% or more of the time across the 8 channels:", nrow(grp2_morethan_50perc)))
    print(paste("Number of nodes in gp3 50% or more of the time across the 8 channels:", nrow(grp3_morethan_50perc)))
    
    grp1_morethan_25perc <- grps_perc_final[grps_perc_final["gp1_inperc"] >= 0.25, ]
    grp2_morethan_25perc <- grps_perc_final[grps_perc_final["gp2_inperc"] >= 0.25, ]
    grp3_morethan_25perc <- grps_perc_final[grps_perc_final["gp3_inperc"] >= 0.25, ]
    # print(grp1_morethan_25perc)
    # print(grp2_morethan_25perc)
    # print(grp3_morethan_25perc)

    print(paste("Number of nodes in gp1 25% or more of the time across the 8 channels:", nrow(grp1_morethan_25perc)))
    print(paste("Number of nodes in gp2 25% or more of the time across the 8 channels:", nrow(grp2_morethan_25perc)))
    print(paste("Number of nodes in gp3 25% or more of the time across the 8 channels:", nrow(grp3_morethan_25perc)))

    # Calculate the overlap in all the channels 
        # how many users are in 100 of the channels and how many are in 50% of the channels
    mult_channels <- data.frame(nodes=complete_nodelist,
                             count=rep(NA, length(complete_nodelist)),
                             perc=rep(NA, length(complete_nodelist))
                             )
    mult_channels <- check_multiplechannels(mult_channels, channel_list)
    nodes_in50ormore_channels <- mult_channels[ mult_channels["perc"] >= 0.5,]
    print(nodes_in50ormore_channels)
    nodes_inmore50_channels <- mult_channels[ mult_channels["perc"] > 0.5,]
    print(nodes_inmore50_channels)
    print(paste("The fraction of users in more than 50% of the channels: ", nrow(nodes_inmore50_channels)/nrow(mult_channels)))
    print( nrow(nodes_inmore50_channels)) #12
    print( nrow(mult_channels)) #17


}

main(GP1_list, GP2_list, GP3_list)