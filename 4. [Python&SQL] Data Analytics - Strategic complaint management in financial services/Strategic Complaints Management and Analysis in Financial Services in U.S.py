#import modules and data preprocessing in order to load data into MySQL
import MySQLdb
import pandas as pd
import re
import matplotlib.pyplot as plt


df = pd.read_csv("complaints.csv")


#drop column 'Date received', 'Consumer complaint narrative' 
df = df.drop(['Date received', 'Consumer complaint narrative'], axis=1)



#Transform Complaint ID to int type
df["Complaint ID"] = df["Complaint ID"].astype("int")
#Replace repeat values in Product column
df['Product'] = df['Product'].replace('Credit reporting', 'Credit reporting, credit repair services, or other personal consumer reports')
df['Product'] = df['Product'].replace('Credit card', 'Credit card or prepaid card')
df['Product'] = df['Product'].replace('Prepaid card', 'Credit card or prepaid card')
df['Product'] = df['Product'].replace('Payday loan', 'Payday loan, title loan, or personal loan')
df['Product'] = df['Product'].replace('Money transfers', 'Money transfer, virtual currency, or money service')
df['Product'] = df['Product'].replace('Virtual currency', 'Money transfer, virtual currency, or money service')


# Except for NaN in column ZIP_Code, NaN are replaced with 'unknown'
df.loc[:, df.columns != 'ZIP code'] = df.loc[:, df.columns != 'ZIP code'].fillna('unknown')
df["ZIP code"] = df["ZIP code"].fillna(0)


# define a function to ensure zip code has 5 numbers or it should be zero
def clean_zip_code(value):
    if re.match(r'\d{5}', str(value)) is None:
        return 0
    else:
        return value

# use apply() to update zip code column
df['ZIP code'] = df['ZIP code'].apply(clean_zip_code)
df["ZIP code"] = df["ZIP code"].astype("int")



#Show unique value for each column
columns = df.columns.tolist()
for column in columns:
    unique_values = df[column].unique()
    num_unique = df[column].nunique()
    print(f"Column '{column}'unique()result:\n{unique_values}")
    print(num_unique," unique values \n")




#Show type of each column
print(df.dtypes)

print("========================================================")

# Use groupby to Product column, and use nunique by Product group to gain the counts of unique value of issues
issue_counts = df.groupby('Product')['Issue'].nunique()

print(issue_counts)




#%% Create database in MySQL


try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           port=3306)
    #Use cursor() to work in MySQL
    cursor = conn.cursor()
    
    #Create database ComplaintsProject
    sql = """CREATE DATABASE IF NOT EXISTS Final_ComplaintsProject"""
    cursor.execute(sql)
    print("Create database successfully")
except:
    print("Fail to create database")
    
finally:
    conn.close()
    print("Stop connecting to database")


#%% Create table in MySQL


try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           database = "Final_ComplaintsProject",
                           port=3306)
    #Use cursor() to work in MySQL
    cursor = conn.cursor()    
    #Create table
    table = """CREATE TABLE IF NOT EXISTS Complaints(Product VARCHAR(200),
                                                    Sub_Product VARCHAR(100),
                                                    Issue VARCHAR(200),
                                                    Sub_Issue VARCHAR(100),
                                                    Company_Public_Response VARCHAR(200),
                                                    Comapny VARCHAR(100) NOT NULL,
                                                    State VARCHAR(1000),
                                                    ZIP_Code int(10),
                                                    Tags VARCHAR(100),
                                                    Consumer_Consent_Provided VARCHAR(100),
                                                    Submitted_Via VARCHAR(50),
                                                    Complaints_Date DATE NOT NULL,
                                                    Company_Response_To_Consumer VARCHAR(100),
                                                    Timely_Response VARCHAR(10),
                                                    Consumer_Disputed VARCHAR(50),
                                                    ID INT(10) NOT NULL PRIMARY KEY)"""
    cursor.execute(table)
    print("Create table columns successfully")
    
    #Put data into table
    try:
        for i in range(len(df)):
            sql = """INSERT INTO Complaints(Product, Sub_Product, Issue, Sub_Issue, Company_Public_Response, Comapny, State, ZIP_Code, Tags, Consumer_Consent_Provided, Submitted_Via, Complaints_Date, Company_Response_To_Consumer, Timely_Response, Consumer_Disputed, ID)
                                VALUES(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"""
            var = (df.iloc[i,0], df.iloc[i,1], df.iloc[i,2], df.iloc[i,3], df.iloc[i,4], df.iloc[i,5], df.iloc[i,6], df.iloc[i,7], df.iloc[i,8], df.iloc[i,9], df.iloc[i,10], df.iloc[i,11], df.iloc[i,2], df.iloc[i,13], df.iloc[i,14], df.iloc[i,15])
            cursor.execute(sql, var)
        
        print("Insert data successfully")
        conn.commit()
    except Exception as e:
        print("Error:",e)
        
except Exception as e:
    print("Error:",e)
    print("Fail to connect database")

finally:
    conn.close()
    print("Stop connecting to database")


#%% MySQL Data cleaning


try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           database = "Final_ComplaintsProject",
                           port=3306,
                           charset = "utf8")
    #Use cursor() to work in MySQL
    cursor = conn.cursor() 
    
    #Data cleaning
    try:
        #Delete data from year 2011-2015 and 2023(due to incomplete data in every months)
        cursor.execute("""DELETE FROM Complaints WHERE Complaints_Date > '2022-12-31'""")
        cursor.execute("""DELETE FROM Complaints WHERE Complaints_Date BETWEEN '2011-01-01' AND '2015-12-31'""")
        
        conn.commit()
        print("Clean data successfully")
    except Exception as e:
        print("Error:", e)

except Exception as e:
    print("Error:",e)
    print("Fail to connect database")
    
finally:
    conn.close()
    print("Stop connecting to database")



#%% Select year data from MySQL to plot "Number of complaints per year"

try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           database = "Final_ComplaintsProject",
                           port=3306,
                           charset = "utf8")
    #Use cursor() to work in MySQL
    cursor = conn.cursor() 
    
    try:
        sql = """SELECT YEAR(Complaints_Date) AS complaint_year,
                COUNT(*) AS complaint_count
                FROM Complaints
                Where State != 'unknown'
                GROUP BY complaint_year"""
        cursor.execute(sql)
        year_data = cursor.fetchall()
        print("Select finished")
        
        
    except Exception as e:
        print("Error:", e)
    
    
    
except Exception as e:
    print("Fail to connect database：", e)

finally:
    conn.close()
    print("Stop connecting to database")


#%%


#Plot "Number of complaints per year"

#year_data type transform to list, and sorted by year
year_data = list(year_data)
year_data.sort(key=lambda x: x[0])

#Set x, y data
growth_rate=[0]

year = [i[0] for i in year_data]

number = [(j[1])/10000 for j in year_data]

for k in range(len(number)):
    try:
        growth = ((number[k+1]-number[k])/number[k])*100
        growth_rate.append(round(growth,1))
    except:
        break



#Plotting and set title
fig, ax = plt.subplots(figsize=(6,4))
ax.bar(year, number, color="peru", alpha=0.5)  #alpha: transparency
ax2 = ax.twinx()
ax2.set_ylabel("Year growth rate of complaints")
ax.plot(year, growth_rate, color="brown", marker="o")
plt.title("Yearly Complaint Trends and Growth Rates")
fig.tight_layout()


#Set x,y labels
ax.set_xlabel("year")
ax.set_ylabel("number of complaints(10k)")
plt.xticks(year)

#Save chart
fig = plt.gcf
plt.savefig("Yearly Complaint Trends and Growth Rates.png", bbox_inches='tight', dpi=300)

#Show chart
plt.show()
print("Show the graph")



#%% Select year data from MySQL to plot "Number of complaints per year"

try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           database = "Final_ComplaintsProject",
                           port=3306,
                           charset = "utf8")
    #Use cursor() to work in MySQL
    cursor = conn.cursor() 
    
    try:
        sql_2 = """SELECT Product,
                COUNT(*) AS Product_count
                FROM Complaints
                GROUP BY Product"""
        cursor.execute(sql_2)
        product_data = list(cursor.fetchall())
        print("Select finished")
        
        
    except Exception as e:
        print("Error:", e)
    
    
    
except Exception as e:
    print("Fail to connect database：", e)

finally:
    conn.close()
    print("Stop connecting to database")




#%%

#Plot "Percentage of different types of products"

#product_data sorted by number of different types of products
product_data.sort(key=lambda x: -x[1])

#Set pie chart data
products_type = [i[0] for i in product_data]
products_type5 = products_type[0:5].copy()
products_type5.append('Others')

products_number = [(j[1]) for j in product_data]
products_number5 = products_number[0:5].copy()
total = 0  
for i in range(5, len(products_number)):  
    total += products_number[i]  # sum from 7th number
products_number5.append(total)
    
colors = ['crimson', 'cornflowerblue', 'violet', 'orange', 'darkkhaki',(0.8, 1, 0.8)]
explode = [0.1,0,0,0,0,0]


#Plotting and set title
fig, ax = plt.subplots(figsize=(8, 4))
_, _, autopcts = ax.pie(products_number5, labels = products_type5, colors = colors, 
        explode = explode, autopct = '%1.1f%%', pctdistance=0.7, textprops={'fontsize': 13, 'color': 'black'}, startangle=90, counterclock=False)
autopcts[0].set_fontsize(23)

plt.axis("equal")
ax.set_title("Percentage of different complained products in USA", fontsize=22, loc='center')

# adjust location
fig.subplots_adjust(left=0.2, right=0.8, bottom=0.2, top=1.6)

#Save chart
fig = plt.gcf
plt.savefig("Percentage of different complained products in USA.png", bbox_inches='tight', dpi=300)

#show plot
plt.show()
print("Show the graph")



#%% To watch unique value of State using SQL to output Excel
import MySQLdb
import pandas as pd

#Connect to MySQL and database
con = MySQLdb.connect(host='localhost', user=input("user:"),
password=input("passward:"), database="Final_ComplaintsProject",
port=3306,charset = "utf8")

# Select
df = pd.read_sql_query("SELECT DISTINCT State FROM complaints;", con)

# save csv
df.to_csv('State unique value.csv', index=False)

# Close database connection
con.close()



#%% Select complaints data by region, product

try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           database = "Final_ComplaintsProject",
                           port=3306,
                           charset = "utf8")
    #Use cursor() to work in MySQL
    cursor = conn.cursor() 
    
    try:
        sql_3 = """SELECT 
                        CASE 
                            WHEN state IN ('UNITED STATES MINOR OUTLYING ISLANDS','HI','AS', 'GU', 'MP', 'PR', 'VI', 'PW', 'FM', 'MH', 'PW') THEN 'Pacific'
                            WHEN state IN ('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'PA', 'NJ') THEN 'Northeast'
                            WHEN state IN ('WI', 'MI', 'IL', 'IN', 'OH', 'ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO') THEN 'Midwest'
                            WHEN state IN ('DE', 'MD', 'DC', 'VA', 'WV', 'NC', 'SC', 'GA') THEN 'South'
                            WHEN state IN ('FL', 'KY', 'TN', 'AL', 'MS', 'AR', 'LA', 'OK', 'TX') THEN 'Southeast'
                            WHEN state IN ('MT', 'ID', 'WY', 'CO', 'NM', 'AZ', 'UT', 'NV', 'CA') THEN 'West'
                            ELSE 'Northwest'
                        END AS region,
                        Product,
                        COUNT(*) AS complaint_count
                    FROM complaints
                    WHERE state NOT IN ('unknown', 'AE', 'AP', 'AA')
                    GROUP BY region, Product;
                """
        cursor.execute(sql_3)
        state_data = list(cursor.fetchall())
        print("Select finished")
        
        
    except Exception as e:
        print("Error:", e)
    
    
    
except Exception as e:
    print("Fail to connect database：", e)

finally:
    conn.close()
    print("Stop connecting to database")



#%% Set pie chart data using state_data

midwest_list = []
northeast_list = []
northwest_list = []
pacific_list = []
south_list = []
west_list = []
southeast_list = []



for i in state_data:
    if i[0] == 'Midwest':
        midwest_list.append((i[1],i[2]))
    if i[0] == 'Northeast':
        northeast_list.append((i[1],i[2]))
    if i[0] == 'Northwest':
        northwest_list.append((i[1],i[2]))
    if i[0] == 'Pacific':
        pacific_list.append((i[1],i[2]))
    if i[0] == 'South':
        south_list.append((i[1],i[2]))
    if i[0] == 'West':
        west_list.append((i[1],i[2]))
    if i[0] == 'Southeast':
        southeast_list.append((i[1],i[2]))


midwest_total=0
midwest_list.sort(key=lambda x: -x[1])
midwest_top = midwest_list[:5]
for j in midwest_list[5:12]:
    midwest_total += j[1]
midwest_top.append(('Others',midwest_total))


northeast_total=0
northeast_list.sort(key=lambda x: -x[1])
northeast_top = northeast_list[:5]
for j in northeast_list[5:12]:
    northeast_total += j[1]
northeast_top.append(('Others',northeast_total))


northwest_total=0
northwest_list.sort(key=lambda x: -x[1])
northwest_top = northwest_list[:5]
for j in northwest_list[5:12]:
    northwest_total += j[1]
northwest_top.append(('Others',northwest_total))


pacific_total=0
pacific_list.sort(key=lambda x: -x[1])
pacific_top = pacific_list[:5]
for j in pacific_list[5:12]:
    pacific_total += j[1]
pacific_top.append(('Others',pacific_total))


south_total=0
south_list.sort(key=lambda x: -x[1])
south_top = south_list[:5]
for j in south_list[5:12]:
    south_total += j[1]
south_top.append(('Others',south_total))


west_total=0
west_list.sort(key=lambda x: -x[1])
west_top = west_list[:5]
for j in west_list[5:12]:
    west_total += j[1]
west_top.append(('Others',west_total))


southeast_total=0
southeast_list.sort(key=lambda x: -x[1])
southeast_top = southeast_list[:5]
for j in southeast_list[5:12]:
    southeast_total += j[1]
southeast_top.append(('Others',southeast_total))


#%% Select complaints data by region

try:
    #Connect to MySQL and database
    conn = MySQLdb.connect(host="localhost",
                           user=input("user:"),
                           password=input("passward:"),
                           database = "Final_ComplaintsProject",
                           port=3306,
                           charset = "utf8")
    #Use cursor() to work in MySQL
    cursor = conn.cursor() 
    
    try:
        sql_4 = """SELECT 
                        CASE 
                            WHEN state IN ('UNITED STATES MINOR OUTLYING ISLANDS','HI','AS', 'GU', 'MP', 'PR', 'VI', 'PW', 'FM', 'MH', 'PW') THEN 'Pacific'
                            WHEN state IN ('ME', 'NH', 'VT', 'MA', 'RI', 'CT', 'NY', 'PA', 'NJ') THEN 'Northeast'
                            WHEN state IN ('WI', 'MI', 'IL', 'IN', 'OH', 'ND', 'SD', 'NE', 'KS', 'MN', 'IA', 'MO') THEN 'Midwest'
                            WHEN state IN ('DE', 'MD', 'DC', 'VA', 'WV', 'NC', 'SC', 'GA') THEN 'South'
                            WHEN state IN ('FL', 'KY', 'TN', 'AL', 'MS', 'AR', 'LA', 'OK', 'TX') THEN 'Southeast'
                            WHEN state IN ('MT', 'ID', 'WY', 'CO', 'NM', 'AZ', 'UT', 'NV', 'CA') THEN 'West'
                            ELSE 'Northwest'
                        END AS region,
                        COUNT(*) AS total_complaint_count
                    FROM complaints
                    WHERE state NOT IN ('unknown', 'AE', 'AP', 'AA')
                    GROUP BY region"""
        cursor.execute(sql_4)
        regiontotal_data = list(cursor.fetchall())
        print("Select finished")
        
        
    except Exception as e:
        print("Error:", e)
    
    
    
except Exception as e:
    print("Fail to connect database：", e)

finally:
    conn.close()
    print("Stop connecting to database")



#%% To watch unique value of Product using SQL to output Excel
import MySQLdb
import pandas as pd

# Connect to MySQL and database
con = MySQLdb.connect(host='localhost', user=input("user:"),
password=input("passward:"), database="Final_ComplaintsProject",
port=3306,charset = "utf8")

# Select
df = pd.read_sql_query("SELECT DISTINCT Product FROM complaints;", con)

# save csv
df.to_csv('Product unique value.csv', index=False)

# Close database connection
con.close()




#%% Plot USA map and pie chart
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches


# Create a Basemap object with specified map range and projection
m = Basemap(llcrnrlon=-130, llcrnrlat=15, urcrnrlon=-55, urcrnrlat=50, projection='lcc', lat_1=33, lat_2=45, lon_0=-95)

# Draw state boundaries
m.drawstates()


# Draw coastlines
m.drawcoastlines()

# Draw country boundaries
m.drawcountries()

# Draw rivers
m.drawrivers()

# Set the title of the plot
plt.title('Percentage of different complained products by regions')

 


product_colors = {
    'Mortgage': 'violet',
    'Debt collection': 'cornflowerblue',
    'Credit reporting, credit repair services, or other personal consumer reports': 'crimson',
    'Credit card or prepaid card': 'orange',
    'Checking or savings account':'darkkhaki',
    'Others':(0.8, 1, 0.8)
}

# Define complaints data for different regions
region_data = {'Northeast': northeast_top, 'Midwest': midwest_top, 'South': south_top, 'West': west_top, 'Northwest': northwest_top, 'Pacific': pacific_top,'Southeast': southeast_top}



# Define the shortened product names and colors
product_colors = {
    'Credit issues': 'crimson',
    'Debt collection': 'cornflowerblue',
    'Mortgage': 'violet',
    'Credit/prepaid card': 'orange',
    'Checking/savings account':'darkkhaki',
    'Others':(0.8, 1, 0.8)
}

# Create a mapping of the original product names to the shortened product names
shortened_product_names = {
    'Credit reporting, credit repair services, or other personal consumer reports': 'Credit issues',
    'Debt collection': 'Debt collection',
    'Mortgage': 'Mortgage',
    'Credit card or prepaid card': 'Credit/prepaid card',
    'Checking or savings account':'Checking/savings account',
    'Others':'Others'
}

# Update region_data with the shortened product names
shortened_region_data = {}
for region, data in region_data.items():
    shortened_data = [(shortened_product_names[product], value) for product, value in data]
    shortened_region_data[region] = shortened_data

# Use shortened_region_data instead of region_data in your original code
region_data = shortened_region_data

# Create legend elements with the shortened product names
legend_elements = [mpatches.Patch(color=color, label=product) 
                   for product, color in product_colors.items()]

# Add the legend to your plot
plt.legend(handles=legend_elements, loc='upper left', prop={'size': 4})




# Extract the complaint counts and normalize them for pie chart size
complaint_counts = [row[1] for row in regiontotal_data]
max_complaints = max(complaint_counts)
sizes = [300 * (count / max_complaints) for count in complaint_counts]


# Create a dictionary that maps regions to sizes
region_sizes = {
    'Midwest': sizes[0],
    'Northeast': sizes[1],
    'Northwest': sizes[2]*3,
    'Pacific': sizes[3]*8,
    'South': sizes[4],
    'Southeast': sizes[5],
    'West': sizes[6],
}


# Plot pie charts for different regions
for region, data in region_data.items():
    labels, values = zip(*data)  # unpack to two lists: product names, complaint counts
    colors = [product_colors[label] for label in labels]  # get colors for each product
    size = region_sizes[region]  # look up the size for this region
    
    # determine the axes for the pie chart based on the region
    if region == 'Midwest':
        ax = plt.axes([0.45, 0.55, 0.14, 0.14])
    elif region == 'Northeast':
        ax = plt.axes([0.6, 0.55, 0.14, 0.14])
    elif region == 'Northwest':
        ax = plt.axes([0.3, 0.6, 0.14, 0.14]) 
    elif region == 'Pacific':
        ax = plt.axes([0.15, 0.3, 0.14, 0.14])
    elif region == 'South':
        ax = plt.axes([0.5, 0.35, 0.14, 0.14])
    elif region == 'Southeast':
        ax = plt.axes([0.7, 0.3, 0.14, 0.14])
    elif region == 'West':
        ax = plt.axes([0.3, 0.4, 0.14, 0.14])
        
    if region in ['Northwest']:
        pie = ax.pie(values, colors=colors, autopct='%1d%%', pctdistance=0.9, radius = size/100, startangle=90, counterclock=False)
    elif region in ['Pacific']:
        pie = ax.pie(values, colors=colors, autopct='%1d%%', pctdistance=2, radius = size/100, startangle=90, counterclock=False)
    else:
        pie = ax.pie(values, colors=colors, autopct='%1d%%', pctdistance=0.8, radius = size/100, startangle=90, counterclock=False)
    # smaller text
    for text in pie[2]:
        text.set_fontsize(4)      
    ax.text(0.55, 0, region, ha='center', va='center', fontsize=7, transform=ax.transAxes)
    #region name parameter
    

plt.savefig('Percentage of different complained products by regions.png', dpi=300)

# Show the plot
plt.show()


