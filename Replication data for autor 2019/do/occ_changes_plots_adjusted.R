#################################################################################
# This file plots changes in employment shares in different occupation groups. It
# makes figures 3,4, and 5 in the paper.
#
# Input: avg_yearly_occ_wages_shares_hrswt.dta
# Output: occ_group_pct_change_`subgroup'.pdf, skill_group_lvl_change_`subgroup'.pdf
#################################################################################

library(data.table)
library(ggplot2)
library(ggthemes)
library(readstata13)
library(stringi)

# sink('../log/occ_changes_plots.log')

data_path <- './Replication data for autor 2019/dta/avg_yearly_occ_wages_shares_hrswt.dta'
figures_dir <- '../fig/'

make_filename <- function(subgroup, group='occ',change='lvl'){
	# Makes plot filename
	#
	# Args:
	#   subgroup: demographic subgroup to plot (e.g. coll_m)
	#   group: what is being plotted (e.g. occ groups or skill groups)
	#   change: type of change (levels or percent)
	#
	# Returns: plot filename
	filename <- paste0(figures_dir,group,'_group_',change,'_change_',subgroup,'.png')
	return(filename)
}

combine_occs <- function(df,subgroup){
	# Combines transportation, construction, and farming into one laborer occupation.
  # Combines personal services and health services into one services occupation.
	#
	# Args:
	#   df: yearly occupational employment data
	#   subgroup: demographic subgroup (e.g. coll_m)
	#
	# Returns: data frame with combined occupations, saves plot
	if (subgroup=='overall') {
		df[, sh_labr := sh_tran+sh_come+sh_farm]
		df[, sh_hlpr := sh_sper+sh_shel]
		df[, c('sh_tran','sh_come','sh_farm') := NULL]
		df[, c('sh_sper','sh_shel') := NULL]
	} else {
	  labor_occ <- paste0('sh_labr_',subgroup)
		labor_occs <- paste0(c('sh_tran','sh_come','sh_farm'),'_',subgroup)
		df[, (labor_occ) := 0]
		for(occ in labor_occs){
			df[, (labor_occ) := df[[labor_occ]]+df[[occ]]]
		}

		services_occ <- paste0('sh_hlpr_',subgroup)
		service_occs <- paste0(c('sh_sper','sh_shel'),'_',subgroup)
		df[, (services_occ) := 0]
		for(occ in service_occs){
			df[, (services_occ) := df[[services_occ]]+df[[occ]]]
		}
		df[, (labor_occs) := NULL]
		df[, (service_occs) := NULL]
	}

	return(df)
}

get_subgroup_columns <- function(df,subgroup){
	# Gets the columns corresponding to a specific subgroup (e.g. coll_m)
	#
	# Args:
	#   df: yearly occupational employment data
	#   subgroup: demographic subgroup (e.g. coll_m)
	#
	# Returns: data frame containing only specified subgroup's columns
	column_regex <- ifelse(subgroup=='overall','sh_.{4}$',paste0('sh_.{4}_',subgroup,'$'))
	cols <- grep(column_regex,names(df),value=TRUE)
	cols <- c('year',cols)
	sdf <- df[,..cols]
	return(sdf)
}

calculate_changes <- function(df,intervals,levels=FALSE){
	# Calculates changes in shares in occupations in the provided intervals
  #
  # Args:
  #   df: yearly occupational employment data
  #   intervals: intervals to calculate change in
  #   levels: boolean variable indicating whether to calculate level or proportional change
  #
  # Returns: data with period occupation changes.
	for (interval in intervals){
	  start <- substr(interval,1,4)
	  end <- substr(interval,6,9)
	  change_column <- paste0('d_share_',interval)
		if (levels==TRUE){
			df[,(change_column) := 100*(df[[end]]-df[[start]])]
		} else{
			df[,(change_column) := 100*(df[[end]]-df[[start]])/(df[[start]])]
		}
		if (interval == '2000-2016'){
			df[,(change_column) := df[[change_column]]*10/16]
		}
	}
	return(df)
}

add_skill_level <- function(df,subgroup){
	# Adds broad skill level categories to data (Low, Mid, High)
  #
  # Args:
  #   df: yearly occupational employment data
  #   subgroup: demographic subgroup being plotted (e.g. coll_m)
  #
  # Returns: data with broad skill level categories
	if (subgroup=='overall'){
		df[occ10 %in% c('sh_exma','sh_prof','sh_tech'), Skill := 'High Skill']
		df[occ10 %in% c('sh_sclp','sh_hlpr','sh_labr'),Skill := 'Low Skill']
		df[is.na(Skill), Skill := 'Mid Skill']
	} else{
		high_skill_occs <- paste0(c('sh_exma','sh_prof','sh_tech'),'_',subgroup)
		low_skill_occs <- paste0(c('sh_sclp','sh_hlpr','sh_labr'),'_',subgroup)
		df[occ10 %in% high_skill_occs, Skill := 'High Skill']
		df[occ10 %in% low_skill_occs, Skill := 'Low Skill']
		df[is.na(Skill), Skill := 'Mid Skill']
	}

	return(df)
}

rename_occs <- function(df,occ_titles,subgroup){
	# Renames occupations to their proper titles (e.g. sh_exma to Managers)
  #
  # Args:
  #   df: yearly occupational employment data
  #   occ_titles: list mapping occupational variable names to occupation titles
  #   subgroup: demographic subgroup being plotted (e.g. coll_m)
  #
  # Returns: data with changed occupation titles
	for (occ in names(occ_titles)){
		old_name <- occ_titles[[occ]]

		if(subgroup!='overall'){
			old_name <- paste0(old_name,'_',subgroup)
		}

		df[occ10 == old_name, occ10 := occ]
	}
	df[, occ10 := factor(occ10,levels=names(occ_titles))]
	return(df)
}

plot_occ_group_pct_changes <- function(df,subgroup,subgroup_title,intervals=c('1980-1990','1990-2000','2000-2016')){
  # Plots percentage change in employment in detailed occupational categories
  #
  # Args:
  #   df: yearly occupational employment data
  #   subgroup: demographic subgroup to plot (e.g. coll_m)
  #   subgroup_title: demographic subgroup name, used for plot title (e.g. College Men)
  #   intervals: intervals to plot change for (e.g. 2000-2010, 1990-2000)
  #
  # Returns: (none), saves plot
	sdf <- get_subgroup_columns(df,subgroup)
	sdf <- combine_occs(sdf,subgroup)
	sdf <- melt(sdf,id='year',variable.name='occ10',value.name='share')
	sdf <- dcast(sdf,occ10 ~ year,value.var='share')
	sdf <- calculate_changes(sdf,intervals)

	change_cols <- c('occ10',grep('d_share_*',names(sdf),value=TRUE))
	sdf <- sdf[,..change_cols]

	sdf <- melt(sdf,id='occ10')
	sdf[, Interval := substr(variable,9,17)]
	sdf <- add_skill_level(sdf,subgroup)
	sdf[, Skill := factor(Skill, levels=c('Low Skill','Mid Skill','High Skill'))]
	occ_titles <- list("Health + Personal Svcs"='sh_hlpr',"Clean + Protect Svcs"='sh_sclp',
	"Operator/Labor"='sh_labr',"Production"='sh_prod',"Office/Admin"='sh_adms',"Sales"='sh_rsal',
	'Technicians'='sh_tech','Professionals'='sh_prof','Managers'='sh_exma')

	sdf <- rename_occs(sdf,occ_titles,subgroup)
	interval <- ifelse(subgroup=='overall','1970-2016','1980-2016')
	occ_change_plot <- ggplot(sdf,aes(occ10,value,fill=Skill,alpha=Interval))+
											geom_bar(stat='identity',position='dodge')+
											scale_alpha_discrete(range=c(.2,.6,1),labels=paste(intervals,' '))+theme_stata()+
											labs(x='',y='Change (% pts)')+
											scale_fill_manual(values=c('#d7191c','#fdae61','#2c7bb6'))+
											theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
												axis.text.x=element_text(angle=-45,size=8,hjust=0),axis.text.y=element_text(angle=0),
												legend.title=element_blank(),legend.background = element_rect(colour='white'))+
												guides(fill=FALSE)

	plot_filename <- make_filename(subgroup,group='Fig_3_occ',change='pct')
	ggsave(plot_filename,width=6,height=4.5)
	return(occ_change_plot)
}

plot_skill_group_levels_changes <- function(df,subgroup,subgroup_title,intervals=c('1980-1990','1990-2000','2000-2016')){
	# Plots percentage change in employment in broad skill categories (Low, Mid, High)
  #
  # Args:
  #   df: yearly occupational employment data
  #   subgroup: demographic subgroup to plot (e.g. coll_m)
  #   subgroup_title: demographic subgroup name, used for plot title (e.g. College Men)
  #   intervals: intervals to plot change for (e.g. 2000-2010, 1990-2000)
  #
  # Returns: (none), saves plot
	sdf <- get_subgroup_columns(df,subgroup)
	sdf <- combine_occs(sdf,subgroup)
	sdf <- melt(sdf,id='year',variable.name='occ10',value.name='share')
	sdf <- add_skill_level(sdf,subgroup)
	sdf <- sdf[,.(share=sum(share)),by=.(year,Skill)]
	sdf <- dcast(sdf,Skill ~ year,value.var='share')
	sdf <- calculate_changes(sdf,intervals,levels=TRUE)

	change_cols <- c('Skill',grep('d_share_*',names(sdf),value=TRUE))
	sdf <- sdf[,..change_cols]

	sdf <- melt(sdf,id='Skill')
	sdf[, Interval := substr(variable,9,17)]
	sdf[, Skill := factor(Skill, levels=c('Low Skill','Mid Skill','High Skill'))]
	skill_labels <- c(' Health, Personal, Cleaning &   \n Security, Operators, Laborers  ',' Production, Clerical,   \n Admin, Sales   ',' Professional, \n Technical, Managerial')
	interval <- ifelse(length(intervals)==4,'1970-2016','1980-2016')
	occ_change_plot <- ggplot(sdf,aes(Skill,value,fill=Skill,alpha=Interval))+
											geom_bar(stat='identity',position='dodge')+
											scale_alpha_discrete(range=c(.2,.6,1),labels=paste(intervals,' '))+theme_stata()+
											labs(title=paste('Changes in Occupational Employment Shares,',interval),x='',y='',subtitle=paste(subgroup_title,'(Changes in Shares in Pct Points) per Decade'))+
											scale_fill_manual(values=c('#d7191c','#fdae61','#2c7bb6'),labels=skill_labels)+
											theme(axis.ticks.x = element_blank(), plot.background = element_rect(fill='white'),
											axis.text.y=element_text(angle=0),legend.title=element_blank(),
											legend.background = element_rect(colour='white'))+
											guides(fill=guide_legend(order=1),alpha=guide_legend(order=99))


	if(subgroup == 'overall'){
		plot_filename <- make_filename(subgroup,group='Fig_4_skill',change='lvl')
	}else{
		plot_filename <- make_filename(subgroup,group='Fig_5_skill',change='lvl')
	}
	ggsave(plot_filename,width=7,height=5.25)
}

# Main Function
df <- as.data.table(read.dta13(data_path))

# Figure 3
plot_occ_group_pct_changes(df,subgroup='overall',subgroup_title='Working Age Adults',
										intervals=c('1970-1980','1980-1990','1990-2000','2000-2016'))

# Figure 4
plot_skill_group_levels_changes(df,subgroup='overall',subgroup_title='Working Age Adults',
									  intervals=c('1970-1980','1980-1990','1990-2000','2000-2016'))

# Figure 5
plot_skill_group_levels_changes(df,subgroup='c',subgroup_title='College',
									             intervals=c('1980-1990','1990-2000','2000-2016'))

plot_skill_group_levels_changes(df,subgroup='nc',subgroup_title='Non-College',
									             intervals=c('1980-1990','1990-2000','2000-2016'))

sink()
