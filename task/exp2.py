
'''
Code for the task used in Experiment 2 of
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

save_path = 'continuous_precision_e2_data/'

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
lure_im = visual.SimpleImageStim(win, image='images/island.png')
study_area = visual.Circle(win, radius=500, edges=64, fillColor=CIRCCOL, lineColor=CIRCCOL)

object_list_main = [each for each in os.listdir('images/') if each.endswith('.png') and each != 'island.png' and not each.startswith(".")]
object_list = [object_list_main[i] for i in range(216)]
object_list_prac = [object_list_main[i] for i in range(216, 216+18, 1)]

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
in a location with the word 'STUDY' above it. For test events you will have to identify an object that you saw before from a set of two and \
then recall its location using the mouse. During this phase the word 'TEST' will appear on screen.\n\nLeft click to continue"

OPENING_INSTR_4 = "'STUDY' events will require you to remember the object and its exact location. 'TEST' events will require you to \
first identify the object you studied from a choice of two. To select the object you remember left click on it. It is important to \
do this quickly as you only have 4 seconds for this part.\n\nLeft click to continue"

OPENING_INSTR_5 = "Once you have selected the object you remember, the second part of the 'TEST' event will require you to move \
the mouse to the location that the object previously appeared in and left click. You will have 6 seconds to do this part of the task, \
so try to be as fast and as accurate as possible.\n\nLeft click to continue"

OPENING_INSTR_6 = "You will now have a chance to practice this task. Remember to respond quickly during the test phase as the computer will \
move on and mark the response as incorrect.\n\nLeft click to continue"


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
        x = x - WINSIZE[0]/2
        y = y - WINSIZE[1]/2
        myMouse.setPos((x,y))


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


def addToEvents(event_list, image_list, lure_list):

    if len(image_list) != len(event_list)/3:
        raise(Warning("Error in addToEvents: numbers of events, images, and/or locations dont match"))
    if len(image_list) != len(lure_list):
        raise(Warning("Error in addToEvents: numbers of studied images and lures should match"))

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
            event_list[e]['lure']=lure_list[N_studied] # only used later during test 1
            for t in range(e, len(event_list), 1):
                if event_list[t]['event'] in ['test1', 'test2'] and event_list[t]['lag'] == event_list[e]['lag'] and event_list[t]['trial'] == event_list[e]['trial']:
                    event_list[t]['loc']= used_locations[N_studied]
                    event_list[t]['image']=image_list[N_studied]
                    event_list[t]['lure']=lure_list[N_studied]
                    try:
                        # now location has been probed, remove from list
                        occupied_locations.pop(used_locations[N_studied])
                    except:
                        pass
            N_studied+=1

    return(event_list)

def halfList(list_obj, first_half = True):
    ll = len(list_obj)
    h1 = [list_obj[i] for i in range(0,ll/2, 1)]
    h2 = [list_obj[i] for i in range(ll/2, ll, 1)]

    if first_half:
        return(h1)
    else:
        return(h2)

def createBlockLists():
    lags = random.sample(range(len(l.lag_lists)), 6)

    # split images
    random.shuffle(object_list)
    obj_1 = halfList(object_list, True)
    obj_2 = halfList(object_list, False)

    list_1 = []
    list_2 = []
    for lag in range(len(lags)):
        if lag % 2 == 0:
            list_1.extend(l.lag_lists[lag])
        else:
            list_2.extend(l.lag_lists[lag])

    ### Add studied object and location
    # list 1
    list_1 = addToEvents(event_list=list_1, image_list=halfList(obj_1, True), lure_list = halfList(obj_1, False))

    # list 2
    list_2 = addToEvents(event_list=list_2, image_list=halfList(obj_2, True), lure_list = halfList(obj_2, False))

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


def testEvent(image, lure, tafc_time = 4, reloc_time = 6):
    # get location response
    object_im.setImage('images/' + image)
    lure_im.setImage('images/' + lure)

    # set the image locations
    tafc_xsep = 75

    target_left = random.choice([0,1])
    if target_left == 1:
        object_im.setPos([-tafc_xsep, 0])
        lure_im.setPos([tafc_xsep, 0])
    else:
        object_im.setPos([tafc_xsep, 0])
        lure_im.setPos([-tafc_xsep, 0])

    # two-alternative forced choice (tafc)
    moveMouse(WINSIZE[0]/2, WINSIZE[1]/2)
    RT.reset()
    myMouse.clickReset()
    prompt.setText("TEST")
    tafc_choiceMade = False
    timer = core.CountdownTimer(tafc_time)
    win.setMouseVisible(True)
    while not tafc_choiceMade and timer.getTime() > 0:
        mouse1 = 0
        while mouse1 < 1 and timer.getTime() > 0:
            for key in event.getKeys():
                if key in [QUITKEY]:
                    core.quit()
            study_area.draw()
            prompt.draw()
            object_im.draw()
            lure_im.draw()
            win.flip()
            mouse_pos = myMouse.getPos()
            mouse1, mouse2, mouse3 = myMouse.getPressed()
            # wait for button to be released
            while myMouse.getPressed()[0] and timer.getTime() > 0:
                pass
        # was a valid response given?
        if abs(-tafc_xsep - mouse_pos[0]) < 50 and mouse_pos[1] < 50 and mouse1 > 0:
            resp_left = 1
            tafc_choiceMade = True
        elif abs(tafc_xsep - mouse_pos[0]) < 50 and mouse_pos[1] < 50 and mouse1 > 0:
            resp_left = 0
            tafc_choiceMade = True
        else: # valid choice wasnt made
            tafc_choiceMade = False
            mouse1 = 0
        if tafc_choiceMade == True:
            win.setMouseVisible(False)
            moveMouse(WINSIZE[0]/2, WINSIZE[1]/2)
            tafc_rt = RT.getTime()
            while timer.getTime() > 0:
                study_area.draw()
                win.flip()
    if not tafc_choiceMade:
        # if the time ran out
        tafc_rt = 'NA'
        resp_left = "NA"

    win.setMouseVisible(False)
    moveMouse(WINSIZE[0]/2, WINSIZE[1]/2)

    # if a choice was made set the chosen image
    if resp_left != "NA":
        if target_left != resp_left: # they chose the lure...
            object_im.setImage('images/' + lure)

    if resp_left == "NA": # if they didnt make a choice we have to remind them to respond faster
        prompt.setText("PLEASE RESPOND FASTER")
        prompt.setColor([1,-1,-1])
        prompt.draw()
        win.flip()
        core.wait(reloc_time)
        prompt.setColor(BACKGROUND)
        # add NAs to this part of the data
        reloc_rt = "NA"
        mouse_pos = ["NA", "NA"]
    else:
        # if they did make a response run the relocation part
        RT.reset()
        myMouse.clickReset()
        moveMouse(WINSIZE[0]/2, WINSIZE[1]/2)

        prompt.setText("TEST")
        choiceMade = False
        timer = core.CountdownTimer(reloc_time)
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
                while myMouse.getPressed()[0] and timer.getTime() > 0:
                    pass
            # dont accept clicks outside the valid area
            if numpy.sqrt((mouse_pos[0])**2 + (mouse_pos[1])**2)>500:
                choiceMade = False
                mouse1 = 0
            else:
                choiceMade=True
                reloc_rt = RT.getTime()
                while timer.getTime() > 0:
                    study_area.draw()
                    fixation.draw()
                    object_im.draw()
                    win.flip()
        if choiceMade == False:
            # if the time ran out
            reloc_rt = 'NA'

    return(mouse_pos, reloc_rt, target_left, resp_left, tafc_rt)


def runPractice(repeat_option=True, IEI = 0):

    prac_locs = sampleLocations(N=len(object_list_prac)/2, sep=IMSIZE)
    prac_list = addToEvents(event_list=l.prac_list, image_list=halfList(object_list_prac, True), lure_list=halfList(object_list_prac, False))

    practice_done = False
    while not practice_done:

        clickToBegin(text = 'Click to begin practice')

        runBlock(block_list=prac_list, IEI=IEI, write=False)

        text_item.setText("End of practice.\n\nIf you would like to repeat the practice press 'y'. To continue to the main experiment press 'n'.")
        text_item.draw()
        win.flip()
        key = event.waitKeys(keyList=['y','n'])

        if key == ['y']:
            practice_done = False
        if key == ['n']:
            practice_done = True


def runBlock(block_list, block_no=1, IEI=0, write=True):
    # IEI = inter event interval
    for event in block_list:
        if event['event'] == 'study':

            studiedLoc = event['loc']
            studyEvent(image=event['image'], loc=studiedLoc)

            if write:
                data_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(block_no, IEI, event['event'], event['lag'], \
                studiedLoc[0], studiedLoc[1], event['image'], "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"))
                # "block, IEI, eventType, lag, studiedX, studiedY, image, lure, targetLeft, respLeft, tafcACC, tafcRT, recalledX, recalledY, recallError, recallRT\n"

            study_area.draw()
            fixation.draw()
            win.flip()
            core.wait(IEI) # add a delay after each event

        if event['event'] == 'test1':

            mouse_pos, reloc_rt, target_left, resp_left, tafc_rt = testEvent(image=event['image'], lure=event['lure'])

            # calculate error etc and write
            studiedLoc = event['loc']
            if mouse_pos[0] != "NA":
                error = numpy.sqrt((studiedLoc[0] - mouse_pos[0])**2 + (studiedLoc[1] - mouse_pos[1])**2)
            else:
                error = "NA"

            if resp_left != "NA":
                tafc_acc = int(target_left == resp_left)
            else:
                tafc_acc = "NA"

            if write:
                data_file.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' %(block_no, IEI, event['event'], event['lag'], \
                studiedLoc[0], studiedLoc[1], event['image'], event['lure'], target_left, resp_left, tafc_acc, str(tafc_rt), mouse_pos[0], mouse_pos[1], error, str(reloc_rt)))
                # "block, IEI, eventType, lag, studiedX, studiedY, image, lure, targetLeft, respLeft, tafcACC, tafcRT, recalledX, recalledY, recallError, recallRT\n"

            study_area.draw()
            fixation.draw()
            win.flip()
            core.wait(IEI)

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

    col_headers = "block, interEventInt, eventType, lag, studiedX, studiedY, image, lure, targetLeft, respLeft, tafcACC, tafcRT, recalledX, recalledY, recallError, recallRT\n" #"block, lag, studiedX, studiedY, image, recalledX, recalledY, recallError, recallRT, conf, confRT\n"
    global data_file
    file_name = save_path + 'precision_e2_p' + str(expInfo['Participant']) + '_g' + str(expInfo['Group']) + '_'  + expInfo['dateStr']
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
    clickToBegin(text=OPENING_INSTR_6)

    # practice
    runPractice(IEI=1)

    # randomize order of inter event intervals
    iei_order = [0,2]
    random.shuffle(iei_order)

    # run block 1
    clickToBegin(text="Block 1\n\nWhen you are ready to begin this block click the mouse.") # This block will last approximately 13 minutes
    runBlock(block_list=list_1, block_no=1, IEI=iei_order[0])

    # break
    clickToBegin(text="End of block 1\n\nYou now have a chance to take a break\n\nClick to continue to the second block.")

    # run block 2
    runBlock(block_list=list_2, block_no=2, IEI=iei_order[1])

    # thanks + goodbye
    clickToBegin(text="End of the Experiment\n\nThank you for taking part!\n\nClick to exit")

# run the experiment
main()


#end
