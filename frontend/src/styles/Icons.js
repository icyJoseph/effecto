import React from "react";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import {
  faStopwatch,
  faBell,
  faCalendarAlt,
  faMapMarker,
  faEnvelope,
  faPlusCircle,
  faLock,
  faLockOpen,
  faFeatherAlt, // soft mode
  faUsers, // participants
  faUserCircle, // host
  faPoll, // polls
  faLink, // connect
  faCheck, // task completed
  faTasks, //completed tasks
  faGrinStars, //very effective
  faMeh, // somewhat
  faAngry, // not
  faLightBulb,
  faStar,
  faFlagCheckered,
  faBullsEye,
  faComments
} from "@fortawesome/free-brands-svg-icons";

export const SetTimer = () => <FontAwesomeIcon icon={faStopwatch} />;
export const Alarm = () => <FontAwesomeIcon icon={faBell} />;
export const Calendar = () => <FontAwesomeIcon icon={faCalendarAlt} />;
export const Location = () => <FontAwesomeIcon icon={faMapMarker} />;
export const Email = () => <FontAwesomeIcon icon={faEnvelope} />;
export const AddTask = () => <FontAwesomeIcon icon={faPlusCircle} />;

export const Lock = () => <FontAwesomeIcon icon={faLock} />;
export const Unlock = () => <FontAwesomeIcon icon={faLockOpen} />;

export const SoftMode = () => <FontAwesomeIcon icon={faFeatherAlt} />;
export const Participants = () => <FontAwesomeIcon icon={faUsers} />;
export const Host = () => <FontAwesomeIcon icon={faUserCircle} />;
export const Polls = () => <FontAwesomeIcon icon={faPoll} />;
export const Connect = () => <FontAwesomeIcon icon={faLink} />;
export const TaskDone = () => <FontAwesomeIcon icon={faCheck} />;
export const TaskList = () => <FontAwesomeIcon icon={faTasks} />;
export const VeryEffective = () => <FontAwesomeIcon icon={faGrinStars} />;
export const Meh = () => <FontAwesomeIcon icon={faMeh} />;
export const NotEffective = () => <FontAwesomeIcon icon={faAngry} />;
export const Idea = () => <FontAwesomeIcon icon={faLightBulb} />;
export const ProTip = () => <FontAwesomeIcon icon={faStar} />;
export const CompleteMeeting = () => <FontAwesomeIcon icon={faFlagCheckered} />;
export const Purpose = () => <FontAwesomeIcon icon={faBullsEye} />;
export const Chat = () => <FontAwesomeIcon icon={faComments} />;
