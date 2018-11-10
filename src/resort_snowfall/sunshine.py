import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


options = webdriver.ChromeOptions()
options.add_argument('headless')

driver = webdriver.Chrome(chrome_options=options)
driver.get("https://www.skibanff.com/conditions")

time.sleep(15)

delay = 15 # seconds
try:
        myElem = WebDriverWait(driver, delay).until(EC.presence_of_element_located((By.ID, 'snow-reports')))
        print "Page is ready!"
        html = driver.find_element_by_tag_name('html').get_attribute('innerHTML')
        with open('/tmp/sunshine.html','w') as f:
            f.write(html.encode('utf-8'))
            print "Wrote output to: /tmp/sunshine.html"
        
except TimeoutException:
        print "Loading took too much time!"




