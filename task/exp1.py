
'''
Code for the task used in Experiment 1 of
"Age differences in the precision of memory at short and long delays"
Rhodes, Abenne, Meierhofer, & Naveh-Benjamin
'''

from psychopy import visual, core, data, event, logging, gui, misc
import numpy, random, os
import lists as l

try:
    import win32api
except:
    pass

save_path = 'continuous_precision_data/'

if not os.path.exists(save_path):
    os.makedirs(save_path)

WINSIZE = [1920, 1080]
BACKGROUND=[-1,-1,-1]
FOREGROUND=[1,1,1]
CIRCCOL= [i/(255.0/2.0) - 1 for i in [173, 216, 230]]
IMSIZE=100
QUITKEY='escape'

win = visual.Window(WINSIZE, units = 'pix', allowGUI= False, color=BACKGROUND, monitor='monitor1')

text_item = visual.TextStim(win, color=FOREGROUND, height=50, wrapWidth=1000)
prompt = visual.TextStim(win, color=BACKGROUND, height=20, wrapWidth=100, pos=[0,60])
fixation = visual.SimpleImageStim(win, image='images/island.png', pos=[0,0])
object_im = visual.SimpleImageStim(win, image='images/island.png')
study_area = visual.Circle(win, radius=500, edges=64, fillColor=CIRCCOL, lineColor=CIRCCOL)

object_list_main = [each for each in os.listdir('images/') if each.endswith('.png') and each != 'island.png' and not each.startswith(".")]
object_list = [object_list_main[i] for i in range(108)]
object_list_prac = [object_list_main[i] for i in range(108, 108+9, 1)]

conf_scale = visual.RatingScale(win, low=0, high=5, singleClick=True, labels=('0','1','2','3','4','5'), scale='0 = No Memory, 5 = Best Memory', tickMarks=range(6), showAccept=False, stretch=1.5, pos=[0,0], markerColor="#00fa9a")

myMouse = event.Mouse(win = win)
RT = core.Clock()

### instructions

OPENING_INSTR_1 = "Welcome to the experiment!\n\nImagine you are sailing around a desert island and the luggage with passenger's personal \
items has fallen overboard. Your task is to remember where each item is located in the water.\n\nLeft click to see \
the desert island and water"

#[Example image]

OPENING_INSTR_2 = "This experiment is split into two blocks with a break in between. In each block you will be presented with images of \
objects in different locations around the desert island. Remember where each object is as you will have to recall its \
location using the mouse later on.\n\nLeft click to continue"

OPENING_INSTR_3 = "In each block we will mix STUDY events and TEST events. For a study event you will see an object you have not seen before \
in a location with the word 'STUDY' above it. For test events you will see an object you have seen before in the middle of \
the screen with the word 'TEST' above it.\n\nLeft click to continue"

OPENING_INSTR_4 = "'STUDY' events will require you to remember the object and its exact location. 'TEST' events will require you to move \
the mouse to the location the object previously appeared in and left click.\n\nLeft click to continue"

OPENING_INSTR_5 = "Following a 'TEST' event you will be asked to rate your confidence in how accurate your recall of the location was \
from 0 (no memory at all) to 5 (best possible memory).\n\nLeft click to see an example of the confidence scale"

#[example confidence scale]

OPENING_INSTR_6 = "For each test event you will have 5 seconds to recall the location and 5 seconds to rate your confidence, so try to be \
as fast and as accurate as possible. You will now have a chance to practice this task.\n\nLeft click to continue"


def clickToBegin(text = 'Click to begin trial'):
    myMouse.clickReset()

    if text != None:
        # set and present text
        text_item.setText(text)
        text_item.draw()
        win.flip()
    # then wait for a click
    while not myMouse.getPressed()[0]:
        # check if the escape key has been pressed
        for key in event.getKeys():
            if key in [QUITKEY]:
                core.quit()
    # wait until key is released
    while myMouse.getPressed()[0]:
        pass


def moveMouse(x, y):
    try:
        win32api.SetCursorPos((x,y))
    except:
        pass


def sampleLocations(N, valid_radius = 500, sep = 50, already_taken = []):
    locs = []

    while len(locs) < N:
        # sample
        x = random.uniform(-valid_radius, valid_radius)
        y = random.uniform(-valid_radius, valid_radius)

        dists = [numpy.sqrt((x - already_taken[i][0])**2 + (y - already_taken[i][1])**2) for i in range(len(already_taken))]
        if numpy.sqrt(x**2 + y**2) > valid_radius-(IMSIZE*numpy.sqrt(2))/2.0 or any([dists[i] < sep for i in range(len(dists))]) or numpy.sqrt(x**2 + y**2) < IMSIZE*numpy.sqrt(2):
            pass
        else:
            locs.append([x,y])
            already_taken.append([x,y])
    return(locs)


def addToEvents(event_list, image_list):

    if len(image_list) != len(event_list)/3:
        raise(Warning("Error in addToEvents: numbers of events, images, and/or locations dont match"))

    used_locations=[]
    occupied_locations=[]
    N_studied = 0
    for e in range(len(event_list)):
        if event_list[e]['event']=='study':
            # select a new location to use
            new_loc = sampleLocations(N = 1, already_taken=occupied_locations)[0]
            used_locations.append(new_loc) # for keepng track of the studied order
            occupied_locations.append(new_loc)
            # add study information
            event_list[e]['loc']=new_loc
            event_list[e]['image']=image_list[N_studied]
            for t in range(e, len(event_list), 1):
                if event_list[t]['event'] in ['test1', 'test2'] and event_list[t]['lag'] == event_list[e]['lag'] and event_list[t]['trial'] == event_list[e]['trial']:
                    event_list[t]['loc']= used_locations[N_studied]
                    event_list[t]['image']=image_list[N_studied]
                    try:
                        # now location has been probed, remove from list
                        occupied_locations.pop(used_locations[N_studied])
                    except:
                        pass
            N_studied+=1

    return(event_list)


def createBlockLists():
    lags = random.sample(range(len(l.lag_lists)), 6)

    # split images
    random.shuffle(object_list)
    obj_1 = [object_list[i] for i in range(0,len(object_list)/2, 1)]
    obj_2 = [object_list[i] for i in range(len(object_list)/2, len(object_list), 1)]

    list_1 = []
    list_2 = []
    for lag in range(len(lags)):
        if lag % 2 == 0:
            list_1.extend(l.lag_lists[lag])
        else:
            list_2.extend(l.lag_lists[lag])

    ### Add studied object and location
    # list 1
    list_1 = addToEvents(event_list=list_1, image_list=obj_1)

    # list 2
    list_2 = addToEvents(event_list=list_2, image_list=obj_2)

    return(list_1, list_2)


def studyEvent(image, loc, study_time = 4.5, isi = .5):
    prompt.setText("STUDY")
    object_im.setImage('images/' + image)
    object_im.setPos(loc)

    study_area.draw()
    fixation.draw()
    prompt.draw()
    object_im.draw()
    win.flip()
    core.wait(study_time)

    study_area.draw()
    fixation.draw()
    win.flip()
    core.wait(isi)


def testEvent(image, test_time = 5):
    # get location response
    object_im.setImage('images/' + image)

    moveMouse(WINSIZE[0]/2, WINSIZE[1]/2)

    RT.reset()
    myMouse.clickReset()

    prompt.setText("TEST")
    choiceMade = False
    timer = core.CountdownTimer(5)
    while not choiceMade and timer.getTime() > 0:
        mouse1 = 0
        while mouse1 < 1 and timer.getTime() > 0:
            for key in event.getKeys():
                if key in [QUITKEY]:
                    core.quit()

            mouse_pos = myMouse.getPos()
            object_im.setPos(mouse_pos)

            study_area.draw()
            fixation.draw()
            prompt.draw()
            # dont draw object if outside the valid area
            if numpy.sqrt((mouse_pos[0])**2 + (mouse_pos[1])**2)>500:
                pass
            else:
                object_im.draw()
            win.flip()

            mouse1, mouse2, mouse3 = myMouse.getPressed()

            # wait for button to be released
            while myMouse.getPressed()[0]:
                pass
        # dont accept clicks outside the valid area
        if numpy.sqrt((mouse_pos[0])**2 + (mouse_pos[1])**2)>500:
            choiceMade = False
            mouse1 = 0
        else:
            choiceMade=True
            response_time = RT.getTime()
            while timer.getTime() > 0:
                study_area.draw()
                fixation.draw()
                object_im.draw()
                win.flip()
    if choiceMade == False:
        # if the time ran out
        response_time = 'NA'

    # get confidence
    win.setMouseVisible(True)
    timer = core.CountdownTimer(5)
    while conf_scale.noResponse and timer.getTime() > 0:
        conf_scale.draw()
        win.flip()

    if conf_scale.noResponse:
        conf = 'NA'
        conf_time = 'NA'
    else:
        conf = conf_scale.getRating()
        conf_time = conf_scale.getRT()
        while timer.getTime() > 0:
            win.flip()

    win.setMouseVisible(False)
    conf_scale.reset()

    return(mouse_pos, response_time, conf, conf_time)


def runPractice(repeat_option=True):

    prac_locs = sampleLocations(N=len(object_list_prac), sep=IMSIZE)
    prac_list = addToEvents(event_list=l.prac_list, image_list=object_list_prac)

    practice_done = False
    while not practice_done:

        clickToBegin(text = 'Click to begin practice')

        runBlock(block_list=prac_list, write=False)

        text_item.setText("End of practice.\n\nIf you would like to repeat the practice press 'y'. To continue to the main experiment press 'n'.")
        text_item.draw()
        win.flip()
        key = event.waitKeys(keyList=['y','n'])

        if key == ['y']:
            practice_done = False
        if key == ['n']:
            practice_done = True


def runBlock(block_list, block_no=1, write=True):

    for event in block_list:
        if event['event'] == 'study':

            studyEvent(image=event['image'], loc=event['loc'])

        if event['event'] == 'test1':

            mouse_pos, response_time, conf, conf_time = testEvent(image=event['image'])

            # calculate error etc and write
            studiedLoc = event['loc']
            error = numpy.sqrt((studiedLoc[0] - mouse_pos[0])**2 + (studiedLoc[1] - mouse_pos[1])**2)
            if write:
                data_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(block_no, event['lag'], \
                studiedLoc[0], studiedLoc[1], event['image'], mouse_pos[0], mouse_pos[1], error, str(response_time), conf, str(conf_time)))
                # "block, lag, studiedX, studiedY, image, recalledX, recalledY, recallError, recallRT, conf, confRT\n"

        if event['event'] == 'test2':
            pass # ignore...


def main():
    # SET UP GUI AND DATA FILES
    win.setMouseVisible(True)

    expInfo = {'Participant' : 1, 'Gender': ['M', 'F'], 'Group' : [1,2]}
    expInfo['dateStr'] = data.getDateStr()

    dlg = gui.DlgFromDict(expInfo, title = "Basic Information", fixed = ['dateStr'], order=['Participant', 'Group', 'Gender'])
    if dlg.OK:
        LogFile = save_path + "Participant info"
        LogFile = open(LogFile+'.csv', 'a')
        LogFile.write('%s,%s,%s,%s\n' %(expInfo['Participant'], expInfo['Gender'], expInfo['Group'], expInfo['dateStr']))
    else:
        core.quit()
    win.setMouseVisible(False)

    col_headers = "block, lag, studiedX, studiedY, image, recalledX, recalledY, recallError, recallRT, conf, confRT\n"
    global data_file
    file_name = save_path + 'precision_p' + str(expInfo['Participant']) + '_g' + str(expInfo['Group']) + '_'  + expInfo['dateStr']
    data_file = open(file_name + '.csv', 'w')
    data_file.write(col_headers)

    list_1, list_2 = createBlockLists()

    # opening instructions
    clickToBegin(text=OPENING_INSTR_1)

    # example study area...
    text_item.setPos([0,-250])
    text_item.setText("Left click to continue")
    study_area.draw()
    fixation.draw()
    text_item.draw()
    win.flip()

    clickToBegin(text = None)
    text_item.setPos([0,0])

    clickToBegin(text=OPENING_INSTR_2)
    clickToBegin(text=OPENING_INSTR_3)
    clickToBegin(text=OPENING_INSTR_4)
    clickToBegin(text=OPENING_INSTR_5)

    # example confidence rating
    text_item.setPos([0,-250])
    text_item.setText("Left click on the scale to continue")
    win.setMouseVisible(True)
    while conf_scale.noResponse or myMouse.getPressed()[0]:
        conf_scale.draw()
        text_item.draw()
        win.flip()
    conf_scale.reset()
    win.setMouseVisible(False)
    text_item.setPos([0,0])

    clickToBegin(text=OPENING_INSTR_6)

    # practice
    runPractice()

    # run block 1
    clickToBegin(text="Block 1\n\nWhen you are ready to begin this block click the mouse. This block will last approximately 13 minutes")
    runBlock(block_list=list_1, block_no=1)

    # break
    clickToBegin(text="End of block 1\n\nYou now have a chance to take a break\n\nClick to continue to the second block.")

    # run block 2
    runBlock(block_list=list_2, block_no=2)

    # thanks + goodbye
    clickToBegin(text="End of the Experiment\n\nThank you for taking part!\n\nClick to exit")


main()


#end
