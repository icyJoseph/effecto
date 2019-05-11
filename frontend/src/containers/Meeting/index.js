import React, { useState } from "react";
import styled, { withTheme } from "styled-components";
import {
  SetTimer,
  SoftMode,
  Participants,
  Host,
  Polls,
  AddTask,
  Connect,
  TaskDone,
  TaskList,
  VeryEffective,
  Meh,
  NotEffective,
  Idea,
  ProTip,
  CompleteMeeting,
  Purpose,
  Chat,
  Download,
  VoteUp
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

const Points = [
  { id: 0, name: "Look at statistics of last month's", time: "10 min" },
  { id: 1, name: "Discuss Options", time: "10 min" },
  { id: 2, name: "Take decision on at least 3 new brands", time: "10 min" }
];

export function Meeting({ name }) {
  const [donePoints, addChecked] = useState([]);
  const [rating, setRating] = useState("");

  const checkPoint = id => () => addChecked([...donePoints, id]);

  return (
    <MeetingContent>
      Welcome to the {name || "Meeting"}!
      <span>
        <p>Take decision on new product purchases to the candy shop</p>
      </span>
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
        {Points.map(({ id, name, time }) => (
          <div key={id}>
            <Task onClick={checkPoint(id)} checked={donePoints.includes(id)}>
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
    </MeetingContent>
  );
}

export default withTheme(Meeting);
