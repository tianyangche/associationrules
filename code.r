#1. built-in test.
rules<-apriori(grocery,parameter=list(support=0.005,confidence=0.4))
inspect(head(sort(rules,by="confidence"),5))
plot(rules)


#2. self-implementation.

#a. cal_frequent(dataset,maps,support_threshold)
#	get 1-frequent itemset
function(dataset,maps,support_threshold)
{
    candidate_itemset<-maps
    support_count<-rep(1,ncol(dataset))
    support_count<-t(t(support_count))
    
    
    #calculate support for all itemset
    for(i in 1:ncol(dataset))
        support_count[i,1]<-sum(dataset[,i])
    
    
    
    #bind support_count to the candidate_itemset
    candidate_itemset<-cbind(candidate_itemset,support_count)
    
    #decide which itemsets are frequent
    threshold<-support_threshold * nrow(dataset)
    candidate_itemset<-candidate_itemset[which(candidate_itemset$support_count>=threshold),]
    rownames(candidate_itemset) <- seq(1:nrow(candidate_itemset))
    return (candidate_itemset)
}





#b. check_number(frequent_former)
#	get how many times each goods appear in the former frequent itemset


function(frequent_former)
{
    # check how times k in the frequent_itemset
    k <- (ncol(frequent_former)-1)/2
    print(k)
    a <- 0
    frequent_list <- frequent_former[,2]
    for ( i in 1:k)
    {
        frequent_list <- c(frequent_list,frequent_former[,2*i])
    }
    
    
    frequent_list <- unique(frequent_list)
    frequent_list <- as.matrix(frequent_list)
    count_list <- rep(0,nrow(frequent_list))
    count_list <- as.matrix(count_list)
    frequent_list <- cbind(frequent_list, count_list)
    for ( j in 1:nrow(frequent_list))
    { 
        for ( i in 1:k)
        {
            b <- frequent_former[which(frequent_former[,2*i]==frequent_list[j,1]),]
            a <- a + nrow(b)
        }
        frequent_list[j,2] <- a
        a <- 0
    }
    return (frequent_list)
}




#c.	compare(row1, row2)
#	use to determine whether row1 and row2 are equal. Use in look up the hashtree bucket.
function(row1,row2)
{
    k <- ncol(row1)
    i <-1
    while(i <= k)
    {
        if (row1[i]!=row2[2*i])
        {
            return (FALSE)
        }
        i <- i+1
    }
    return (TRUE)
    
}



#d. create_hash_tree(candidate_itemset)
#	the general function to create hash tree. 
function(candidate_itemset)
{
    row_index<<-1
    result<-create_subroutine(candidate_itemset,1,determine_bucket_size(candidate_itemset,1),1)
    row_index<<-1
    return (result)
}



#e.	create_subroutine(candidate_itemset,level,bucket_size,row_id)
#	a subroutine in create_hash_tree. 
function(candidate_itemset,level,bucket_size,row_id)
{
    if(nrow(candidate_itemset) > bucket_size && level <= floor(ncol(candidate_itemset)/2))
    {
        child1<-candidate_itemset[which(candidate_itemset[,2*level]%%10==1),]
        child2<-candidate_itemset[which(candidate_itemset[,2*level]%%10==2),]
        child3<-candidate_itemset[which(candidate_itemset[,2*level]%%10==3),]
        child4<-candidate_itemset[which(candidate_itemset[,2*level]%%10==4),]
        child5<-candidate_itemset[which(candidate_itemset[,2*level]%%10==5),]
        child6<-candidate_itemset[which(candidate_itemset[,2*level]%%10==6),]
        child7<-candidate_itemset[which(candidate_itemset[,2*level]%%10==7),]
        child8<-candidate_itemset[which(candidate_itemset[,2*level]%%10==8),]
        child9<-candidate_itemset[which(candidate_itemset[,2*level]%%10==9),]
        child10<-candidate_itemset[which(candidate_itemset[,2*level]%%10==0),]
        
        #this_result<-matrix(c(row_index,level,row_index+1,row_index+2,row_index+3,row_index+4,row_index+5,rep(0,bucket_size)))
        namelist<-paste("Itemset",1:bucket_size,sep=".")
        
        this_result <- matrix(c(row_id,level,row_index+1,row_index+2,row_index+3,row_index+4,row_index+5,row_index+6,row_index+7,row_index+8,row_index+9,row_index+10,rep(0,bucket_size)),nrow=1,dimnames = list(c(row_index),c("node_id","level","child1","child2","child3","child4","child5","child6","child7","child8","child9","child10",namelist)))
        
        row_index_temp <- row_index
        row_index <<-row_index + 10
        
        child1_result<- create_subroutine(child1,level+1,bucket_size,row_index_temp+1)
        child2_result<- create_subroutine(child2,level+1,bucket_size,row_index_temp+2)
        child3_result<- create_subroutine(child3,level+1,bucket_size,row_index_temp+3)
        child4_result<- create_subroutine(child4,level+1,bucket_size,row_index_temp+4)
        child5_result<- create_subroutine(child5,level+1,bucket_size,row_index_temp+5)
        child6_result<- create_subroutine(child6,level+1,bucket_size,row_index_temp+6)
        child7_result<- create_subroutine(child7,level+1,bucket_size,row_index_temp+7)
        child8_result<- create_subroutine(child8,level+1,bucket_size,row_index_temp+8)
        child9_result<- create_subroutine(child9,level+1,bucket_size,row_index_temp+9)
        child10_result<- create_subroutine(child10,level+1,bucket_size,row_index_temp+10)
        final_result<-rbind(this_result,child1_result,child2_result,child3_result,child4_result,child5_result,child6_result,child7_result,child8_result,child9_result,child10_result)
        
        return (final_result)
    }
    else
    {
        namelist<-paste("Itemset",1:bucket_size,sep=".")
        this_result <- matrix(c(row_id,level,0,0,0,0,0,0,0,0,0,0,rep(0,bucket_size)),nrow=1,dimnames = list(c(row_index),c("node_id","level","child1","child2","child3","child4","child5","child6","child7","child8","child9","child10",namelist)))
        namelist<-rownames(candidate_itemset)
        namelist<-as.integer(namelist)
        a<-rep(0,bucket_size-length(namelist))
        namelist<-c(namelist,a)
        b<-bucket_size+12
        #print(length(namelist))
        this_result[13:b] <- this_result[13:b] + namelist
        #print(namelist)
        return (this_result)
    }
}


#f.	determine_bucket_size(candidate_itemset,level)
#	use to determine the minimum bucket size which is sufficient in the hash tree.
function(candidate_itemset,level)
{
    k <- (ncol(candidate_itemset)-1)/2
    if(level == k)
    {
        leaf_size <-0
        for( i in 1:10)
        {
            if(i != 10)
            {
                temp <- candidate_itemset[which(candidate_itemset[,2*level]%%10==i),]
                if(leaf_size < nrow(temp))
                    leaf_size <- nrow(temp)
            }
            else
            {
                temp <- candidate_itemset[which(candidate_itemset[,2*level]%%10==0),]
                if(leaf_size < nrow(temp))
                    leaf_size <- nrow(temp)                
            }
            
        }
        return (leaf_size)
    }
    else
    {
        leaf_size <-0
        for( i in 1:10)
        {
            if(i != 10)
            {
                temp <- candidate_itemset[which(candidate_itemset[,2*level]%%10==i),]
                potential <- determine_bucket_size(temp,level+1)
                if(leaf_size < potential)
                    leaf_size <- potential
            }
            else
            {
                temp <- candidate_itemset[which(candidate_itemset[,2*level]%%10==0),]
                potential <- determine_bucket_size(temp,level+1)
                if(leaf_size < potential)
                    leaf_size <- potential            
            }
        }
        return (leaf_size)
    }
}

#g.	eliminate(frequent_list,candidate_itemset)
#	eliminate candidate k-entry which doesn't satisfy the property that each k-1 subset of a k-entry must appear at least k-1 times in k-1 frequent itemset.
function(frequent_list,candidate_itemset)
{
    k <- (ncol(candidate_itemset)-1)/2
    temp <- candidate_itemset[,2]
    for ( i in 1:k)
    {
        temp <- c(candidate_itemset[,2*i])
    }
    temp <- unique(temp)
    print(temp)
    for ( i in temp)
    {
        #print(i)
        a <- frequent_list[which(frequent_list[,1]==i),]
        a <- as.matrix(a)
        b <- nrow(a)
        #print("!!!!!!!!!!!")
        #print(b)
        if ( b != 0 )
        {
            b<- a[2,]
        }
        #print("!!!!!!!!!!!!")
        if ( b < k-1)
        {
            for ( j in 1:k)
            {
                candidate_itemset <- candidate_itemset[which(candidate_itemset[,2*j]!=i),]
            }
        }
       # print(dim(candidate_itemset))
        
    }
    return (candidate_itemset)
}

#h.	generate_k_subset(data_entry,k)
#	use to create all k-subsets of a given entry
function(data_entry,k)
{
    #   this function is used to create k-itemset in a given entry
    if(ncol(data_entry) == k)
    {
        return (data_entry)
    }
    else if( k == 1 )
    {

        data_entry<-t(data_entry)
        return (data_entry)
    }
    else
    {

        res <- NULL
        num <- ncol(data_entry)-k+1
        for ( i in 1:num)
        {
            a <- data_entry
            for (j in 1:i)
            {
                a <- a[-1]
                a <- as.matrix(a)
                a <- t(a)
            }
            temp<-cbind(data_entry[i],generate_k_subset(a,k-1))
            #print(temp)
            res <-rbind(res,temp)
        }
        return (res)
    }
}

#i.	generate_rule(frequent_itemset,frequent_list,minconf,maps)
#	generate rules whose confidence is larger than minconf.
function(frequent_itemset,frequent_list,minconf,maps)
{
    # This is the main function of generating rules.
    k <- (ncol(frequent_itemset)-1)/2
    frequent_itemset<-frequent_itemset[,seq(2,ncol(frequent_itemset),2)]
    boundary<-k-1
    for ( m in 1:boundary)
    {
        for ( j in 1:nrow(frequent_itemset))
        {
            analysis<-frequent_itemset[j,]            
            colnames(analysis) <- seq(1:ncol(analysis))              
            if (m>=2)
                analysis <- as.matrix(analysis)           
            res <- generate_k_subset(analysis,m)

            rownames(res) <- seq(1:nrow(res))

            for ( i in 1:nrow(res))
            {
    
                remain<-setdiff(analysis,res[i,])
                if( m >= 2)
                {
                remain <- as.matrix(remain)
                remain <- t(remain)
                }
                up_formula <- get_goal_frequent(frequent_list,k,k)
                down_formula <- get_goal_frequent(frequent_list,k-m,k)
                up_support <- get_support(analysis,up_formula)
                down_support <- get_support(remain,down_formula)
                confidence <- up_support/down_support               
                if(confidence >= minconf)
                {

                    remain <- as.numeric(remain)
                    cat(get_real_name(remain,maps), "--->",get_real_name(res[i,],maps),"    ",sep = ".")
                    cat("confidence = ", as.double(confidence),"\n")
                }
            }

        }
    }   
    colnames(analysis) <- seq(1:ncol(analysis))
    res <- generate_k_subset(analysis,1)
    a <- res[1,]
    b <- analysis
    return (NULL)
}


#j.	get_goal_frequent(frequent_list,k,frequent_total_length)
#	use to return the goal k-frequent itemset in the frequent itemset set. Mainly use in generate_rule
function(frequent_list,k,frequent_total_length)
{
    #   This function is used to return the goal k-frequent itemset. 
    res <- frequent_list
    number_index <- frequent_total_length - k
    if( number_index != 0)
    {
        for( i in 1:number_index)
        {
            res <- res[[1]]
        }
    }
    res <- res[[2]]
    #print(res)
    return (res)
}

#k.	get_k_frequent(hash_tree,dataset,candidate_itemset)
#	get final k frequent itemset
function(hash_tree,dataset,candidate_itemset)
{
    result <- support_count(hash_tree,dataset,candidate_itemset)
    result <- result[(which(result[,ncol(result)] >= 200)),]
    return (result)
    
}


#l.	get_real_name(input_entry,maps)
#	get the good's real name. Use to print the qualified rules out.
function(input_entry,maps)
{
    #   this function is used to get the good's name.
    res <- as.matrix(input_entry)
    res <- t(res)
    k <- ncol(res)
    for( i in 1:k )
    {
        res[1,i] <- as.character(maps[which(maps[,2]==res[1,i]),1]) 
    }
    res <- as.vector(res)
    return (res)

}


#m.	get_support(data_entry,frequent_itemset)
#	get the goal support count for data_entry
function(data_entry,frequent_itemset)
{
    #   get the goal support count for data_entry
    
    num_item <- ncol(data_entry)
   # print(num_item)
    res<-frequent_itemset
    for ( n in 1:num_item)
    {
        res <- res[which(res[,n*2] == data_entry[,n]),]
    }
    if(nrow(res)!=0)
        count <- res[ncol(frequent_itemset)]
    else
        count <- 1000000
    return (count)
}


#n.	next_frequent(former_frequent,one_frequent)
#	Get Fk using Fk-1 X F1
function(former_frequent,one_frequent)
{
    #candidate_itemset is the result to be returned
    candidate_itemset<-NULL
    
    
    # Get the candidate_itemset,using Fk = Fk-1 * F1
    # outside loop is Fk-1,inner loop is F1.
    for(i in 1:nrow(former_frequent))
    {
        # in order to reduce the number of computation, I abstract one_temp from one_frequent.
        # only need to compute a subset in one_frequent
        one_temp <- one_frequent[which(one_frequent$id > former_frequent[i,ncol(former_frequent)-1]),]
        if(dim(one_temp)[1] != 0)
        {
            for( j in 1:nrow(one_temp))
            {
                index1 <- ncol(former_frequent)
                index2 <- ncol(one_temp)
                if(former_frequent[i,index1-1] < one_temp[j,index2-1])
                {
                    temp<-cbind(former_frequent[i,-index1],   one_temp[j,-index2])
                    candidate_itemset<-rbind(candidate_itemset,temp)
                }
            }
        }
    }
    count<-rep(0,nrow(candidate_itemset))
    candidate_itemset<-cbind(candidate_itemset,count)
    rownames(candidate_itemset)<-seq(1:nrow(candidate_itemset))
    return (candidate_itemset)
}


#o.	select_qualified_transaction(dataset,candidate_itemset)
#	use to select transaction whose number of items is larger than or equal to k.
function(dataset,candidate_itemset)
{
    #   This function is responsible for support count
    #   1. eliminate transactions whose number of items is less than K
    qualified_itemset <- dataset
    k <- (ncol(candidate_itemset)-1)/2
    row.sums<-apply(qualified_itemset,1,sum)
    row.sums<-t(t(row.sums))
    qualified_itemset <- cbind(qualified_itemset,row.sums)
    colnames(qualified_itemset)[ncol(qualified_itemset)] <- "rowsum"
    #        child1<-candidate_itemset[which(candidate_itemset[,2*level]%%5==1),]
    qualified_itemset <- qualified_itemset[ which(qualified_itemset[,ncol(qualified_itemset)] >= k) ,]
    qualified_itemset <- qualified_itemset[,-ncol(qualified_itemset)]
    return (qualified_itemset)
}


#p.	support_count(hash_tree,dataset,candidate_itemset)
#	the main part of this program. do support count using the hash tree. Eliminate ones whose support count is less than support_threshold * nrow(database)
function(hash_tree,dataset,candidate_itemset)
{
    #   This function is responsible for support count
    #   1. eliminate transactions whose number of items is less than K
    #   select_qualified_transaction() is responsible for this action
    qualified_itemset <- select_qualified_transaction(dataset,candidate_itemset)
    colnames(qualified_itemset) <- seq(1:ncol(qualified_itemset))
    
    k <- (ncol(candidate_itemset)-1)/2
    print(k)
    #   get each row to analyse
    for(i in 1:nrow(qualified_itemset))
    {
        if(i%%100==0)
        {
        print("i")
        print(i)
        }
        #   process current row to get the ids of items
        analysis <- qualified_itemset[i,]
        analysis <- as.matrix(analysis)
        analysis <- analysis[which(analysis[,1]==1),]
        analysis <- as.matrix(analysis)
        analysis <- t(analysis)        
        analysis[1,] <- colnames(analysis)
        colnames(analysis) <- seq(1:ncol(analysis))
        analysis <- as.matrix(analysis)
        #print(analysis)
       # return (NULL)
        #   get all k-subsets of the current row under analysis
        subset <- generate_k_subset(analysis,k)
        #   print(subset)
        for( j in 1:nrow(subset))
        {
            # preprocess each row in the subset
            each_subset <- subset[j,]
            each_subset <- as.integer(each_subset)
            each_subset <- as.matrix(each_subset)
            each_subset <- t(each_subset)
            #print(each_subset)
            # use hash tree
            goal_node_id <- 1
            goal_row <- hash_tree[which(hash_tree[,1]==goal_node_id),]
            while(goal_row[3]!=0)
            {
                reminder <- each_subset[goal_row[2]]%%10
                #print(reminder)
                if(reminder==0)
                    reminder <-reminder + 10
                goal_node_id <- reminder + 2
                goal_node_id <- goal_row[goal_node_id]
                goal_row <- hash_tree[which(hash_tree[,1]==goal_node_id),]
                #return (each_subset)
            }
            for (m in 13:ncol(hash_tree))
            {
                candidate_index <- goal_row[m]
                #print(candidate_index)
                if(goal_row[m]!=0)
                {
                    if(compare(each_subset,candidate_itemset[candidate_index,])==TRUE){
                        candidate_itemset[candidate_index,ncol(candidate_itemset)] <- candidate_itemset[candidate_index,ncol(candidate_itemset)] +1

                    }
                }
            }

        }
        
    }

    return (candidate_itemset)
}


#q.	tche_generate_rule(dataset,maps,support_threshold,minconf)
#	my main function to generate rules
function(dataset,maps,support_threshold,minconf)
{
    #   This function is the general function to generate_rule, it has some basic components
    #   1. create candidate_itemset next_frequent
    #   2. create hashtree
    #   3. support_count
    #   4. generate_rule
    frequent_1 <- cal_frequent(dataset,maps,support_threshold)
    frequent_list <- frequent_1
    frequent_list <- list(frequent_list,  frequent_1)
    frequent_current <- frequent_1
    frequent_current <- next_frequent(frequent_current,frequent_1)
    tree_level <- 2
    
    #   generate_rule using while loop
    while(nrow(frequent_current) >= 1)
    {

        #bucket_size <- 2 * floor(nrow(frequent_current)/(10 ^ tree_level))
        if (tree_level == 2)
            hashtree_current <- create_hash_tree(frequent_current, 100 )
        else
            hashtree_current <- create_hash_tree(frequent_current, 50 )
        result <- support_count(hashtree_current,dataset,frequent_current)
        result <- result[which(result[,ncol(result)] >= support_threshold * nrow(dataset)),]
        print(result)
        frequent_current <- result
        if(nrow(frequent_current) < 1)
            break
        rownames(frequent_current) <- seq(1:nrow(frequent_current))
        frequent_list <- list(frequent_list,frequent_current)
        frequent_current <- next_frequent(frequent_current,frequent_1)
        tree_level <- tree_level + 1
    }
    
    k_max <- floor(ncol(frequent_list[[2]])/2)
    print(k_max)
    for ( j in 2:k_max)
    {
        frequent_current <- get_goal_frequent(frequent_list,j,k_max)
        generate_rule(frequent_current, frequent_list,minconf,maps)
    }

    return (frequent_list)
}

