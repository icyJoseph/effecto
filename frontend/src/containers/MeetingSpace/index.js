import React, { useState, useRef } from "react";
import styled, { withTheme } from "styled-components";
import { useSelector } from "react-redux";
import {
  SetTimer,
  SoftMode,
  Participants,
  Host,
  Polls,
  AddTask,
  VeryEffective,
  Meh,
  NotEffective,
  Download
} from "../../styles/Icons";

const Row = styled.div`
  display: flex;
  flex-wrap: wrap;
  width: 75%;
  justify-content: center;

  > * {
    margin: 0.5em 2em;
    text-align: center;
    flex: 1;
  }

  > input {
    min-width: 300px;
    padding-left: 0.25em;
    border: none;
    border-bottom: 1px solid black;
    background: transparent;
    font-family: inherit;
    font-size: inherit;
  }
`;

const Rate = styled.div`
  font-size: 1.5em;
  cursor: pointer;
  color: ${({ theme, selected }) => (selected ? theme.darkBlue : "inherit")};
`;

const Task = styled.span`
  position: relative;
  &:before {
    color: ${({ theme, checked }) => (checked ? theme.green : theme.red)};
    content: "•";
    font-size: 5em;
    padding-right: 1.1225em;
    position: absolute;
    top: -100%;
    left: -15%;
  }
`;

const MidRow = styled.div`
  display: flex;
  flex-wrap: wrap;
  flex-direction: column;

  > * {
    margin: 2em;
    text-align: center;
    display: flex;
    flex-direction: column;

    > textarea {
      font-family: inherit;
      font-size: inherit;
    }
  }
`;

const MeetingContent = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  flex: 1;
  width: 100%;
  min-height: 100vh;

  > .centered {
    width: 100%;
    text-align: center;
  }
`;

export function MeetingSpace({ name, startMeeting, sendMessage }) {
  const _message = useRef("");
  const [donePoints, addChecked] = useState([]);
  const [rating, setRating] = useState("");

  const { id: meetingId, messages = [], agenda = [], ...rest } = useSelector(
    ({ meetings: { meeting } }) => meeting
  );

  console.log(rest);
  const checkPoint = id => () => addChecked([...donePoints, id]);

  const broadcast = e => {
    e.preventDefault();
    const value = _message.current.value;
    value.trim() && sendMessage(meetingId, _message.current.value);
    _message.current.value = "";
  };

  return (
    <MeetingContent>
      Welcome to the {name || "Meeting"}!
      <span>
        <p>Take decision on new product purchases to the candy shop</p>
      </span>
      <div onClick={() => startMeeting(meetingId)}>Click here to start</div>
      <Row>
        <div>
          <div>Mode</div>
          <SoftMode />
        </div>
        <div>
          <div>Participants</div>
          <Participants />
        </div>
        <div>
          <div>Host</div>
          <Host />
        </div>
      </Row>
      <Row>
        <div>
          <div>Add Poll</div>
          <Polls />
        </div>
        <div>
          <div>Assign Task</div>
          <AddTask />
        </div>
        <div>
          <div>Download</div>
          <Download />
        </div>
      </Row>
      <MidRow>
        {agenda.map(({ id, title: name, time }, index) => (
          <div key={`${name}-${index}`}>
            <Task
              onClick={checkPoint(`${name}-${index}`)}
              checked={donePoints.includes(`${name}-${index}`)}
            >
              <span>{name}</span> - <span>{time}</span> <SetTimer />
            </Task>
            <textarea type="text" placeholder="Notes" />
          </div>
        ))}
      </MidRow>
      <Row>
        <div>Rate this Meeting</div>
      </Row>
      <Row>
        <Rate selected={rating === "bad"} onClick={() => setRating("bad")}>
          <div>Bad</div>
          <NotEffective />
        </Rate>
        <Rate selected={rating === "meh"} onClick={() => setRating("meh")}>
          <div>Meh</div>
          <Meh />
        </Rate>
        <Rate selected={rating === "good"} onClick={() => setRating("good")}>
          <div>Effective</div>
          <VeryEffective />
        </Rate>
      </Row>
      <Row>
        <form onSubmit={broadcast}>
          <input type="text" ref={_message} placeholder="Say something" />
          <button>Send</button>
        </form>
      </Row>
      {messages.map(({ user, message }, index) => (
        <div key={index}>
          <div>{user}:</div>
          <div>{message}</div>
        </div>
      ))}
    </MeetingContent>
  );
}

export default withTheme(MeetingSpace);
